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


-- | the end-to-end construct-flatten test for control-flow graphs
module Test.ConstructFlatten where

import CFG.Common
import CFG.Construct
import CFG.Flatten
import Test.Common
import Inliner
import Data.Graph.Inductive.Graph
import Data.Default
import Data.Data
--import Language.ECMAScript5.PrettyPrint
import SyntaxHelpers.AnnotatedPrint
import Language.ECMAScript5.Syntax
import Language.ECMAScript5.Syntax.Arbitrary
import Text.PrettyPrint.HughesPJ
import Control.Monad.State

testConstructFlatten :: (Default a, Data a, Eq a, Show a) => JavaScript a -> Bool
testConstructFlatten js = 
  let dsjs = desugar js in
  let wpg = construct dsjs in
  case flatten wpg of
    Left err -> False
    Right fjs -> (removeNodeIds fjs) == dsjs

testConstructFlattenTimed js = within 10000000 (testConstructFlatten js)g

testConstructFlattenShrinking js = 
  forAll (elements $ shrink js) testConstructFlatten
  
testcase1 = Script () [DoWhileStmt () (LabelledStmt () (Id () "\NUL:") (EmptyStmt ())) (NumLit () (-2.388122398693717))]
dtestcase1 = desugar testcase1
dstmts1 = let Script _ stmts = dtestcase1 in stmts
wpg1 = construct dtestcase1
cfg1 = let WPG top _ = wpg1 in top
ftestcase1 = flatten wpg1

t4 = BlockStmt () [EmptyStmt (), EmptyStmt ()]
