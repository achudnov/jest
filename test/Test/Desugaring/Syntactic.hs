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


module Test.Desugaring.Syntactic where

import Test.QuickCheck
import Test.Tasty.QuickCheck
import Language.ECMAScript5.Syntax
import Language.ECMAScript5.Syntax.Arbitrary
import Inliner.Desugaring
import Data.Generics.Uniplate.Data
import Data.Default.Instances.Base
import Test.JSQuickCheck
import Test.Executable
import Control.Monad
import Test.Tasty

test_desugaring_syntactic :: TestTree
test_desugaring_syntactic =testGroup "Desugaring syntactic correctness properties" tests

tests :: [TestTree]
tests = [testProperty "VarDecls are only present at the beginning of the function body or script and the only initializers allowed are functions" prop_vds
        ,testProperty "No function statements in the program" prop_noFunStmts
        ]

prop_vds :: Program () -> Bool
prop_vds js =
  let djs@(Program _ body) = desugar js
  in f body && and [f fbd| FuncExpr () _ _ fbd <- universeBi djs]
  where f :: [Statement ()] -> Bool
        f = g True
        g :: Bool -> [Statement ()] -> Bool
        g _ [] = True
        g expectVarDecls (s:ss) = case s of
          VarDeclStmt _ vds ->
            expectVarDecls && and (map checkVD vds) && g expectVarDecls ss
          _ -> g False (tail $ universe s) && g False ss
        -- if a VarDecl has an initializer, it can only be a function expression
        checkVD (VarDecl _ _ Nothing ) = True
        checkVD (VarDecl _ _ (Just (FuncExpr {}))) = True
        checkVD _ = False

prop_noFunStmts :: Program () -> Bool
prop_noFunStmts js =  let djs = desugar js
                      in null [f |f@(FunctionStmt () _ _ _) <- universeBi djs]
