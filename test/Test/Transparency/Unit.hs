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


{-# LANGUAGE QuasiQuotes, TupleSections, TemplateHaskell #-}

-- | Unit tests for transparency (primarily, of the monitor run-time
-- and the monitor operations): one case per syntactic production
module Test.Transparency.Unit where

import Test.Tasty
import Test.TaRTuFfe
import Test.JSQuickCheck
import Test.Executable
import Test.Common
import RTS.Level
import Language.ECMAScript5.Syntax
import Language.ECMAScript5.Syntax.QuasiQuote
import Test.Transparency.Generator
import Test.Transparency.Common
import Control.Monad

test_transparency_unit :: IO TestTree
test_transparency_unit =
  liftM (testGroup "Transparency Unit Tests (MRTS transparency)") $
  liftM (map mkTest) test_cases
                         
mkTest :: TRCase -> TestTree
mkTest trc = mkJSTest (caseName trc) (compileTestCase $ compileTRCase trc) qcOutputCheck

test_cases :: IO [TRCase]
test_cases = liftM2 (++) cases_expressions cases_statements

-- Aliases for generators
any = arbValue
string = arbString
bool = arbBool
null = arbNull
whole = arbWholeNum
int = arbInt
float = arbFloatUnit
date = arbDate
undef = arbUndef
char = arbChar
array = arbValArray
object = arbObject

cases_expressions :: IO [TRCase]
cases_expressions = mapM (uncurry3 mkTransCase)
  [("var", ["x" ::: arbValue], [js|return x;|])
  --,("this", [], [js|return this;|]) -- 'This' equality wouldn't be
                                      -- satisfied if it's a global object
  ,("dot", ["x" ::: arbValue], [js|var o = {}; o.f = x; return o.f;|])
  ,("bracket", ["f" ::: arbString, "x" ::: arbValue], 
   [js|var o = {}; o[f] = x; return o[f];|])
  ,("cond", ["x" ::: arbValue, "y" ::: arbValue, "z" ::: arbValue],
   [js|return x?y:z;|])
  ,("list", ["x" ::: arbValue, "y" ::: arbValue], [js|return (x, y);|])]
  -- ++ map (\(n, op) -> TRCase n [tp $ "x" ::: arbValue] $
  --         mkOutput $ PrefixExpr () op (VarRef () $ Id () "x")) 
  --       $(enumOps ''PrefixOp)
  -- ++ map (\(n, op) -> TRCase n [tp $ "x" ::: arbValue] $
  --         mkOutput $ UnaryAssignExpr () op (LVar () "x")) 
  --       $(enumOps ''UnaryAssignOp)
  -- ++ map (\(n, op) -> TRCase n (map tp ["x" ::: arbValue, "y" ::: arbValue]) $
  --         mkOutput $ InfixExpr () op (VarRef () $ Id () "x")
  --                                    (VarRef () $ Id () "y")) 
  --       $(enumOps ''InfixOp)
  -- ++ map (\(n, op) -> TRCase n (map tp ["x" ::: arbValue, "y" ::: arbValue]) $
  --         mkOutput $ AssignExpr () op (LVar () "x") (VarRef () $ Id () "y")) 
  --       $(enumOps ''AssignOp)

cases_statements :: IO [TRCase]
cases_statements = return []
