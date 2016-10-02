-- This file is part of JEST.
-- 
-- JEST is free software: you can redistribute it and/or modify it under
-- the terms of the GNU General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your
-- option) any later version.
-- 
-- JEST is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE.  See the LICENSE for more details.
-- 
-- A copy of the GNU General Public License should have been included
-- along with JEST in a file named LICENSE. If not, see
-- <http://www.gnu.org/licenses/>.


{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Specification and construction of executable JavaScript
-- QuickCheck (qc.js) tests.

module Test.JSQuickCheck (TestCase (..)
                         ,Config (..)
                         ,Parameter (..)
                         ,Environment (..)
                         ,compileTestCase
                         ,arbLevelGen
                         ,lowLevelGen
                         ,arbObject
                         ,arbValue
                         ,arbValArray
                         ,arbString
                         ,arbBool
                         ,arbNull
                         ,arbWholeNum
                         ,arbInt
                         ,arbFloatUnit
                         ,arbDate
                         ,arbChar
                         ,arbUndef
                         ,qcOutputCheck
                         ,Arbitrary (..)
                         ,assert
                         ,parameter_name
                         ,arbName
                         ,mkQCTest
                         ) where

import Language.ECMAScript5.Syntax
import Language.ECMAScript5.Syntax.Annotations
import Language.ECMAScript5.Syntax.CodeGen hiding (switch)
import Language.ECMAScript5.Syntax.QuasiQuote
import Language.ECMAScript5.PrettyPrint
import Language.ECMAScript5.Parser hiding (program)
import Data.Default.Class
import Data.Default.Instances.Base
import Data.List
import SyntaxHelpers
import RTS.Level
import Test.JSQuickCheck.QCJS
import Test.Tasty
import Test.Tasty.Options
import Test.Tasty.Providers
import Data.Proxy
import Data.Typeable (Typeable)
import Test.Executable
import System.IO
import Test.Common
import Control.Applicative
import Data.Monoid
import Options.Applicative hiding (Success, value)
import Data.Set (Set)
import qualified Data.Set as Set (fromList)
import Control.Arrow
import Data.Int
import Data.Maybe

-- | Assert checker for determining whether an executable JS
-- QuickCheck test succeeded
qcOutputCheck :: String -> Bool
qcOutputCheck out = take 4 out == "Pass"

assert :: Default a => Expression a -> Expression a
assert e = CallExpr def (DotRef def (VarRef def $ Id def "c") (Id def "assert")) [e]

-- | Defines a parameter of the test case: the parameter name and the Arbitrary object
data Parameter = String ::: Arbitrary

parameter_arb (_ ::: a) = a
parameter_name (n ::: _) = n

data Config = Config {max_tests :: Int32
                     ,max_invalid :: Int32
                     ,max_shrinks :: Int32
                     }

data TestCase = TestCase {description :: String
                         ,extra_dependencies :: Program ()
                         ,parameters :: [Parameter]
                         ,config :: Config
                         ,body :: [Statement ()]
                         ,environment :: Environment
                         }
                deriving Typeable

instance IsTest TestCase where
  run opts tc _ =
    let JSQCIsolateTestCases isolate = lookupOption opts
        JSQCSavedSourcePrefix prefix = lookupOption opts
    in  runJS "js" (compileTestCase tc) >>= \res -> case res of
          Success out -> case parseQCOutput out of
            Pass -> return $ testPassed ""
            Fail args -> if isolate then
                           do file <- generateFreshFileName prefix
                              writeFile file $ show $ prettyPrint $ isolateTestCase tc args
                              return $ testFailed $ "Test case failed with input: " ++
                                show (prettyPrint $ ArrayLit () $ map Just args) ++
                                "\nIsolated test case saved in " ++ file
                         else return $ testFailed $ "Test case failed with input: " ++
                              show (prettyPrint $ ArrayLit () $ map Just args)
          Error   err -> return $ testFailed $ "Interpreter error." ++ err
  testOptions = return [Option (Proxy :: Proxy JSQCIsolateTestCases)
                       ,Option (Proxy :: Proxy JSQCSavedSourcePrefix)
                       ]

mkQCTest :: TestCase -> TestTree
mkQCTest tc = singleTest (description tc) tc

data QCResult = Pass
              | Fail [Expression ()]

parseQCOutput :: String -> QCResult
parseQCOutput out = case take 4 out of
  "Pass" -> Pass
  "fail" -> case parse expression "" $ last $ lines out of
             Left err  -> error $ show err
             Right expr -> case expr of
                            ArrayLit _ args -> Fail $ map redef $ catMaybes args
                            _ -> error "Unexpected format of the failing test case"
  _ -> error $ "can't parse qc.js output: " ++ out

isolateTestCase :: TestCase -> [Expression ()] -> Program ()
isolateTestCase tc args =
  program $ [vardecls [VarDecl def "Level" $ hygienic <$> (compileLevelImpl <$> levelImpl)
                                                      <*> (pure $ var internalLevelCtorName)]]
           ++ (unProgram $ extra_dependencies tc) ++
           [expr $ call (compilePropertyDeclaration (parameters tc) (body tc)) $
            caseStub:adaptedArgs]
  where adaptedArgs = map adapt args
        caseStub = object [value (propId "assert") $ lambda ["x"] $ udef $
                           [js|if (!x) throw ("AssertFailed");|]
                          ,value (propId "guard") $ lambda ["x"] $ udef $
                           [js|if (!x) throw ("InvalidCase");|]
                          ,value (propId "classify") (lambda [] [])
                          ,value (propId "collect")  (lambda [] [])
                          ,value (propId "noteArg") (lambda [] [])]
        adapt e | isLevel e = call (lambda []
                                    [vardecls [varinit "l" $ new (var "Level") []]
                                    ,expr $ assign ((var "l") `dot` "level") $
                                     fromJust $ valueOfTheLevelField e
                                    ,returns $ var "l"
                                    ]) []
        adapt e@(ObjectLit a fields) | isBox e =
           ObjectLit a $ map adaptPropAssign fields
        adapt e = e
        adaptPropAssign pa = case pa of
          PValue a p e -> PValue a p $ adapt e
          _            -> pa
        levelImpl = case levels of
          [] -> Nothing
          _  -> Just $ if same $ map classifyLevelImpl levels then
                         classifyLevelImpl $ head levels
                       else error "Unexpected heterogeneous level implementations in test case"
        classifyLevelImpl e = 
          case valueOfTheLevelField e of
              Just (ObjectLit {})      -> StringSet
              Just (NumLit _ (Left _)) -> BitVector
              _                 -> error "Unexpected level implementation"
        valueOfTheLevelField (ObjectLit _ fields) =
          maybeHead $ catMaybes $ map
          (\f -> case f of
              PValue _ p v | propName p == "level" -> Just v
              _ -> Nothing
          ) fields
        levels = filter isLevel args
        isLevel e  = case e of
          ObjectLit _ fields -> Set.fromList (map (propName . getProp) fields) ==
                                Set.fromList ["level", "join", "leq", "toString"]
                             && (isJust $ valueOfTheLevelField e)
          _ -> False
        isBox e    = case e of
          ObjectLit _ fields -> Set.fromList (map (propName . getProp) fields) ==
                                Set.fromList ["l", "v", "t", "m"]
        isValueField p = case p of
          PValue {} -> True
          _ -> False
        getProp p = case p of
          PValue _ p _ -> p
          PGet _ p _ -> p
          PSet _ p _ _ -> p
        propName f = case f of
          PropId _ n -> n
          PropString _ s -> s
          PropNum _ n -> show n

-- | Produce isolated executable test-cases based on failures
newtype JSQCIsolateTestCases = JSQCIsolateTestCases Bool
                           deriving (Eq, Typeable)

-- | Gives the ability to specify a custom directory prefix for saving
-- isolated test-case sources for QCIsolateTestCases
newtype JSQCSavedSourcePrefix = JSQCSavedSourcePrefix FilePath
                            deriving (Eq, Typeable)

instance IsOption JSQCIsolateTestCases where
  defaultValue = JSQCIsolateTestCases False
  optionName = return "jsqc-isolate"
  optionHelp = return "Isolate test-cases from qc.js failures"
  parseValue = fmap JSQCIsolateTestCases . safeRead
  optionCLParser = JSQCIsolateTestCases <$> switch (long "jsqc-isolate" <> help "Isolate test-cases from qc.js failures")

instance IsOption JSQCSavedSourcePrefix where
  defaultValue = JSQCSavedSourcePrefix "."
  optionName = return "jsqc-prefix"
  optionHelp = return "Prefix for saving isolated test cases source"
  parseValue = fmap JSQCSavedSourcePrefix . safeRead
  optionCLParser = JSQCSavedSourcePrefix <$> strOption (long "jsqc-prefix" <> help "Prefix for saving isolated test cases source")

data Arbitrary = Extra String (Expression ()) [Arbitrary]
               | BuiltIn String

arbName :: Arbitrary -> String
arbName a = case a of
  Extra n _ _ -> n
  BuiltIn n -> n

instance Eq Arbitrary where
  a1 == a2 = arbName a1 == arbName a2

instance Default TestCase where
  def = TestCase {description = ""
                 ,extra_dependencies = Program () []
                 ,parameters = []
                 ,config = Config 100 10 2
                 ,body = []
                 ,environment = Rhino
                 }

data Environment = Firebug -- ^ uses console.log for output. For
                           -- browsers and node.js
                 | Rhino -- ^ uses print for output. For Rhino and
                         -- SpiderMonkey standalone interpreters

arbLevelGen :: LevelImplementation -> Arbitrary
arbLevelGen li = Extra "arbLevel" (li2arb li) [arbInt, arbStringArray]

-- | Convenience constaint generator of labels of low level
lowLevelGen :: LevelImplementation -> Arbitrary
lowLevelGen li = Extra "arbLevel" (redef [jsexpr|{arb: Level.bottom()}|]) []

arbObject = BuiltIn "arbObject"
arbValue = BuiltIn "arbValue"

arbValArray = BuiltIn "arbValArray"
arbStringArray = BuiltIn "stringArrayGen"

arbString = BuiltIn "arbString"
arbBool = BuiltIn "arbBool"
arbNull = BuiltIn "arbNull"
arbWholeNum = BuiltIn "arbWholeNum"
arbInt = BuiltIn "arbInt"
arbFloatUnit = BuiltIn "arbFloatUnit"
arbDate = BuiltIn "arbDate"
arbChar = BuiltIn "arbChar"
arbUndef = BuiltIn "arbUndef"
arbArray = BuiltIn "arbArray"

li2arb :: LevelImplementation -> Expression ()
li2arb li = case li of
  BitVector -> arbBitVector
  StringSet -> arbStringSet
  Custom _  -> error "Arbitrary instances for custom level implementations are not supported"

arbBitVector :: Expression ()
arbBitVector = CallExpr def (FuncExpr def Nothing []
                             [VarDeclStmt def [varinit "Level" $ hygienic (compileLevelImpl BitVector) (var internalLevelCtorName)] 
                             ,ReturnStmt def $ Just $
                              redef [jsexpr|{arb: function (size) {
                                 var lev = new Level ();
                                 lev.level = randRange(0, 0xFFFFFFFF);
                                 return lev;
                                 }
                                 ,shrink: function(size, lev) {
                                             var s = arbInt.shrink(size, lev.level);
                                             return s.map(function(x) {
                                                   var l = new Level ();
                                                   l.level = x;
                                                   return l;     
                                                });
                                          }
                                 }|]
                             ]) [] 

arbStringSet :: Expression ()
arbStringSet = CallExpr def (FuncExpr def Nothing []
                             [VarDeclStmt def [varinit "Level" $ hygienic (compileLevelImpl StringSet) (var internalLevelCtorName)] 
                             ,ReturnStmt def $ Just $
                              redef [jsexpr|{arb: function (size) {
                                     var lev = new Level ();
                                     var fields = stringArrayGen.arb(size);
                                     for(var i = 0; i < fields.length; i++)
                                        lev[fields[i]] = true;
                                     return lev;
                                     }
                             ,shrink: function(size, lev) {
                                      var fields = [];
                                      for (var f in lev.level)
                                         fields.push(f);
                                      fields = stringArrayGen.shrink(size, fields);
                                      return fields.reduce(function(acc, x) {
                                               return (acc[x] = true);
                                             }, new Level ());
                                     }
                             }|]
                             ]) []

compileTestCase :: TestCase -> Program ()
compileTestCase tc = qcjs `combine`
               compileArbInstances (nub $ [arbStringArray, arbValArray, arbObject, arbValue] ++ collectArbitraries tc) `combine`
               --compileArbInstances (collectArbitraries tc) `combine`
               extra_dependencies tc `combine`
               compileTestDeclaration (description tc) (parameters tc) (body tc)
               `combine`
               compileRunStatement (config tc) (environment tc)

collectArbitraries :: TestCase -> [Arbitrary]
collectArbitraries = nub . concatMap (chaseDependencies . parameter_arb) . parameters

chaseDependencies :: Arbitrary -> [Arbitrary]
chaseDependencies a = case a of
  Extra _ _ deps -> a:(concatMap chaseDependencies deps)
  BuiltIn _ -> []

compileArbInstances :: [Arbitrary] -> Program ()
compileArbInstances = Program () . concatMap compileArb 
  where compileArb a = case a of
          Extra name body _ ->
            [VarDeclStmt () [VarDecl () (Id () name) $ Just body]]
          BuiltIn _ -> []

compileTestDeclaration :: String -> [Parameter] -> [Statement ()] -> Program ()
compileTestDeclaration desc params body =
  program [expr $ call (var "declare")
           [string desc, compiledParamsArb
           ,failOnException $ compilePropertyDeclaration params body]]
  where compiledParamsArb = array $ map (Just . var . ident . arbName . parameter_arb) params
        failOnException f = call (var "failOnException") [f]

compileParams params = map (Id () . parameter_name) params

compilePropertyDeclaration params body = lambda ("c":(compileParams params)) body


compileRunStatement :: Config -> Environment -> Program ()
compileRunStatement config env =
  Program () [ExprStmt () $
              CallExpr () (var "runAllProps") [
                new (var "Config") [
                   int $ max_tests config,
                   int $ max_invalid config,
                   int $ max_shrinks config],
                env2listener env]]
  where env2listener :: Environment -> Expression ()
        env2listener env = let v = case env of
                                 Firebug -> "FBCListener"
                                 Rhino   -> "RhinoListener"
                           in NewExpr () (var v) []
