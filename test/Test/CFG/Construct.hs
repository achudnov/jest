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


module Test.CFG.Construct where

import CFG.Common
import CFG.Construct
import Language.ECMAScript5.Syntax hiding (Prop)
import Language.ECMAScript5.Syntax.Annotations
import Language.ECMAScript5.Syntax.Arbitrary
import Test.Common
import Test.Tasty
import Test.Tasty.QuickCheck
import Lens.Simple
import Data.Graph.Inductive.Graph
import Data.Default.Class

testCFGConstruct = testProperty "Control-Flow Graph invariants" prop_CFG

-- The only nodes that can have multiple precedecessors are the entry
-- nodes, NormalReturn, ExceptionalReturn and Ghost.

-- (NOT TESTED) The only
-- nodes that can have multiple successors are the exit nodes
prop_CFG :: [Statement ()] -> Bool
prop_CFG = ufold ((&&) . chkNode) True . view graph . snd . constructCFG . map  (reannotate $ const (def :: CFAAnnot ()))
  where chkNode :: Context NodeLabel () -> Bool
        chkNode ctx = case (length (pre' ctx) > 1, length (suc' ctx) > 1) of
          (True, True) -> False -- Shouldn't have nodes with multiple
                                -- successors and predecessors
          (True, False) -> case lab' ctx of
                             Begin _ -> True
                             NormalReturn -> True
                             ExceptionalReturn -> True
                             Ghost -> True
                             _     -> False
          (False, True) -> True -- case lab' ctx of
                             -- End _ -> True
                             -- _     -> False
          (False, False) -> True
