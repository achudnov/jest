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


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.Common (myTestArgs,
                    myTest,
                    myTestVerbose,
                    udef,
                    module SyntaxHelpers,
                    combine,
                    withMRTS,
                    withMRTSParams,
                    leftToFail,
                    uncurry3,
                    liftParams,
                    isValidBox,
                    generateFreshFileName,
                    same,
                    maybeHead
                   ) where 

import Test.QuickCheck
import Test.QuickCheck.Property
import Language.ECMAScript5.Syntax.Arbitrary
import Language.ECMAScript5.Syntax
import Language.ECMAScript5.Syntax.Annotations
import Language.ECMAScript5.Syntax.QuasiQuote
import Language.ECMAScript5.Syntax.CodeGen
import Data.Default.Class
--import Test.QuickCheck.Test
import Inliner
import Inliner.Monad hiding (environment)
import RTS
import RTS.Level
import RTS.Policy.Syntax
import Data.Traversable (Traversable)
import SyntaxHelpers
import SyntaxHelpers.JSFunc
import System.EasyFile hiding (combine)
import System.Random
import Control.Monad
import Data.Char
import Data.Graph.Inductive.Graph
import Data.Monoid
import Data.Data (Data)
import Data.Typeable (Typeable)
import RTS.Monitor.Core

myTestArgs = Args {replay = Nothing,
                   maxSuccess = 500,
                   maxDiscardRatio = 10,
                   maxSize = 50,
                   chatty = True}

myTestVerbose t = verboseCheckWithResult myTestArgs t

myTest t = quickCheckWith myTestArgs t

-- | run the inliner where failure is reported via 'assertFailure'
runInliner_ :: Inliner r ->  IO r
runInliner_ i = runInliner i >>= \r ->
  case r of
    Left err -> fail $ show err
    Right x  -> return x
  
unitTestLibPre :: Default a => Program a
unitTestLibPre = reannotate (const def) [js|var result = false;|]

unitTestLibPost :: Default a => Program a
unitTestLibPost = reannotate (const def) [js|print(result);|]

addUnitTest :: Default a => Program a -> Program a
addUnitTest js = unitTestLibPre `combine` js `combine` unitTestLibPost

combine :: Default a => Program a -> Program a -> Program a
combine (Program _ ss1) (Program _ ss2) = Program def $ ss1 ++ ss2

udef :: (Default b) => Program a -> [Statement b]
udef = unProgram . redef

-- redef :: (Traversable x, Default a) => x b -> x a
-- redef = reannotate (const def)

leftToFail :: Show e => Either e x -> IO x
leftToFail (Left e)  = fail $ show e
leftToFail (Right x) = return x

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

-- | Wraps the provided code in a monitor environment with initialized
-- RTS. The monitor is guaranteed to be initialized at "monitor". Uses
-- sensible default parameters for the RTS: stop with an exception,
-- trivial policy, standalone run-time environment
withMRTS :: (Default a, Monoid a, Data a, Typeable a) => LevelImplementation -> [Statement a] ->  [String] -> (Expression a -> Expression a) -> Program a -> JSFunc a
withMRTS li = withMRTSParams $ def {stopfn = Exception
                                   ,policy = Policy li ChannelMapTrivial
                                   ,environment = Standalone}

-- | Wraps the provided code in a monitor environment with initialized
-- RTS. The monitor is guaranteed to be initialized at "monitor".
withMRTSParams :: (Default a, Monoid a, Data a, Typeable a) => Params ->[Statement a] ->  [String] -> (Expression a -> Expression a) -> Program a -> JSFunc a
withMRTSParams pars js = addRTSAdvanced pars "monitor" js 

isValidBox :: Default a => Expression a
isValidBox = redef [jsexpr|function isValidBox(b) {
                       var tv = typeof(b.v);
                       var validPrimBox = (tv === "undefined" || tv === "boolean" || tv === "number" || tv === "string" || (tv === "object" && b.v === null)) && b.t === 0 && "m" in b && typeof(b) === "object" && "l" in b;
                       var validPureObjectBox = (tv === "object") && (b.t === 1) && "m" in b && typeof(b.m) === "object" && b.m.class === "Object" && "l" in b;
                       var validFunBox = ((tv === "function" && b.t === 2 && b.m === {}) ||
                                          (tv === "object" && b.t === 3 && "m" in b && typeof(b.m) === "object" && b.m.class === "Function" && typeof(b.m.func)=== "function")) &&
                                          "l" in b;
                       var validArrayBox = tv === "object" && b.t === 5 && "m" in b && typeof(m) === "object" && b.m.class === "Array" && typeof(b.m.length) === "number" && "l" in b;
                       return exactlyOne([validPrimBox, validPureObjectBox, validFunBox, validArrayBox]);
                       function exactlyOne (boolAr) {
                          for (var i = 0; i < boolAr.length; i++) {
                             if (boolAr[i]) return none(boolAr.slice(i+1,boolAr.length));
                          }
                          return false;
                       }
                       function none (boolAr) {
                          for (var i = 0; i < boolAr.length; i++) {
                              if (boolAr[i]) return false;
                          }
                          return true;
                       }
                   }|]

-- | Generates code that lifts named params into boxes in-place;
-- includes the definition of `liftBox` as well
liftParams :: Default a => String -> [String] -> [Statement a]
liftParams mPrefix parNames =
  map (\n -> expr $ var (ident n) `assign` autolowM (var $ ident n) mPrefix) parNames

generateFreshFileName prefix = do newName <- randomString
                                  let newPath = prefix </> (newName ++ ".js")
                                  dfe <- doesFileExist newPath
                                  if dfe then generateFreshFileName prefix
                                    else return newPath

randomString = replicateM 8 randomLetterOrDigit
randomLetterOrDigit = randomRIO (0,61) >>= \ri ->
  return $ chr $ case ri of
                  _ | ri < 26 -> 65 + ri {-uppercase letter-}
                  _ | ri >= 26 && ri < 52 -> 97 + ri - 26 {-lowercase letter-}
                  _ | otherwise -> 48 + ri - 52 {- digit -}

-- checks if a node is a member of the graph
graphMember :: Graph gr => Node -> gr n e -> Bool
graphMember n gr = case match n gr of
  (Nothing, _) -> False
  (Just _,  _) -> True

same :: Eq a => [a] -> Bool
same [] = True
same (x:xs) = and $ map (x==) xs

maybeHead :: [a] -> Maybe a
maybeHead []     = Nothing
maybeHead (a:_)  = Just a
