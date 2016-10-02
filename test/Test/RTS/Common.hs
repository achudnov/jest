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


{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Test.RTS.Common where

import Language.ECMAScript5.Syntax
import Language.ECMAScript5.Syntax.QuasiQuote
import Language.ECMAScript5.Syntax.CodeGen
import Data.Default.Class
import Test.Common
import qualified RTS as R
import Control.Applicative ((<$>))
import qualified Data.Set as Set
import Test.JSQuickCheck
import RTS hiding (environment)
import RTS.Level
import Test.Tasty
import Test.Executable
import Control.Arrow
import qualified SyntaxHelpers.JSFunc as JSFunc

{- Note [Syntactic structure of RTS test cases]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Since QuickCheck generates unboxed values and boxes are varied, it is
easier to lift (or adapt) native values into boxed values than
generate them directly.

The test case looks like this:

   var liftBox = function (p) {
      ...
   }
   var monitor = (function () {...}) (); // RTS init
   // parameter adaptation: x and y are supposed to be boxes
   x = liftBox(x);
   y = liftBox(y);
   ...
   x.l = x.l.join(xl);
   y.l = y.l.join(yl);
   ...
   with (monitor.globalProxy) {
      <test case code>
   };
-}

data JSProp = JSProp String [Parameter] [Statement ()]

mkTest :: LevelImplementation -> JSProp -> TestCase
mkTest li (JSProp name params body) =
  let paramsToBoxArbLevel = filter (\(_ ::: arb) -> arb == arbBox) params
      liftedArbParamNames = map parameter_name paramsToBoxArbLevel
      paramsToBoxLowLevel = filter (\(_ ::: arb) -> arb == lowBox) params
      liftedLowParamNames = map parameter_name paramsToBoxLowLevel
      liftedParams = liftParams "monitor" $ liftedArbParamNames ++ liftedLowParamNames
      mkLevelParamName p = '_':(p ++ "l")
      liftedParamArbLevelParams = map (\n -> mkLevelParamName n ::: arbLevel) liftedArbParamNames
      updateBoxLevels = map (\n -> ExprStmt () $ AssignExpr () OpAssign
                            (dot (var $ ident n) "l") $ call
                            ((var $ ident n) `dot` "l" `dot` "join")
                            [var $ ident $ mkLevelParamName n]) liftedArbParamNames
      testbody = [expr $ JSFunc.call (withMRTS li (liftedParams ++ updateBoxLevels) [] id (program body)) []]
  in TestCase {description = name ++ " " ++ show li
              ,extra_dependencies = Program () [
                vardecls [varinit "isValidBox" isValidBox
                         ,varinit "eq" $ redef 
                           [jsexpr|function eq (l1, l2) {
                               return (l1.leq(l2) && l2.leq(l1));
                   }|]]]
              ,environment = Rhino
              ,config = Config 300 10 2
              ,parameters = map (fixupLevel li) $ params ++ liftedParamArbLevelParams
              ,body = testbody
              }

jspName :: JSProp -> String
jspName (JSProp name _ _) = name

groupByLevelImpl :: [JSProp] -> [TestTree]
groupByLevelImpl props =
  map (\li -> testGroup (show li) $ map (\p -> mkQCTest $ mkTest li p) props)
  [BitVector, StringSet]

testWithLevelImpls :: JSProp -> TestTree
testWithLevelImpls prop = testGroup (jspName prop) $
                          map (\li -> mkQCTest $ mkTest li prop)
                          [BitVector, StringSet]

testWithBitVector :: JSProp -> TestTree
testWithBitVector = mkQCTest . mkTest BitVector

arbLevel :: Arbitrary
arbLevel = BuiltIn "Level"

lowLevel :: Arbitrary
lowLevel = BuiltIn "LowLevel"

arbBox :: Arbitrary
arbBox = BuiltIn "Box"

lowBox :: Arbitrary
lowBox  = BuiltIn "LowBox"

fixupLevel :: LevelImplementation -> Parameter -> Parameter
fixupLevel li (name ::: arb) | arb == arbLevel = name ::: (arbLevelGen li)
fixupLevel li (name ::: arb) | arb == lowLevel = name ::: (lowLevelGen li)
--fixupLevel li (name ::: arb) | arb == arbBox   = name ::: (arbBoxGen li)
fixupLevel _ param = param
