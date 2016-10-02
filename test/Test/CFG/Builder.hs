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


module Test.CFG.Builder where

import Test.QuickCheck
import Test.QuickCheck.Test
import Test.QuickCheck.Property
import CFG.Builder
import Test.Instances
import Test.Helpers
import Data.Graph.Inductive
import CFG.Common
import qualified Data.Set as Set
import MyPrelude
import Control.Monad.State (runState)

prop_modifyGraph_Idempotency cfg = modifyGraph cfg (return ()) == cfg

prop_disconnect_disconnects :: Node -> Gr n e -> Property
prop_disconnect_disconnects node gr =
  (graphMember node gr) ==> 
  let gr2 = disconnect node gr in
  case match node gr2 of
    (Nothing, _) -> False
    (Just (to, _, _, from), _) -> length from == 0
  
entryExitsPreserved cfg1 cfg2 = 
  (entryNode cfg1 == entryNode cfg2) &&
  (exitNodes cfg1 == exitNodes cfg2)
  
thrd = \(_,_,x) -> x
  
prop_newNode_adds :: (Ord n, Eq n) => n -> CFG n e -> Bool
prop_newNode_adds l cfg =
  let cfg2 = modifyGraph cfg (newNode l) in
  let gr = graph cfg in
  let nextNode = head $ unusedNodes gr in
  let gr2 = graph cfg2 in
  entryExitsPreserved cfg cfg2 &&
  (Set.fromList $ labNodes gr) `Set.union` (Set.singleton (nextNode, l)) == Set.fromList (labNodes gr2)
  
prop_newEdge_adds :: (Ord e, Eq e) => e -> Node -> Node -> CFG n e -> Property
prop_newEdge_adds e from to cfg =
  ((from `graphMember` (graph cfg)) && (to `graphMember` (graph cfg))) ==>
  let cfg2 = modifyGraph cfg (newEdge e from to) in
  let gr = graph cfg in
  let gr2 = graph cfg2 in
  entryExitsPreserved cfg cfg2 &&
  ((Set.fromList (labEdges gr)) `Set.union` (Set.singleton (from, to, e)) == (Set.fromList $ labEdges gr2))
  
prop_newEdges_adds :: (Ord e, Eq e) => [(e, Node)] -> Node -> CFG n e -> Property
prop_newEdges_adds edges to cfg =
  ((and (map (\e -> graphMember (snd e) (graph cfg)) edges)) && (to `graphMember` (graph cfg))) ==>
  let cfg2 = modifyGraph cfg (newEdges edges to) in
  let gr = graph cfg in
  let gr2 = graph cfg2 in
  entryExitsPreserved cfg cfg2 &&
  ((Set.fromList (labEdges gr)) `Set.union` 
   (Set.fromList $ map (\(e, from) -> (from, to, e)) edges) == 
   (Set.fromList $ labEdges gr2))
  
prop_glue = errni  

prop_addGraph :: (Show e, Show n, Ord e, Ord n) => 
                 CFG n e -> GraphBuilder n e a -> Bool
prop_addGraph cfg gb = 
  let ((entry, exits), cfg2)  = runState (addGraph gb) cfg in
  cfg `subCFG` cfg2
  

-- a subgraph relationship, to be used as gr1 `subgraph` gr2
subgraph :: (Ord n, Ord e) => Gr n e -> Gr n e -> Bool
subgraph gr1 gr2 =
  ((Set.fromList $ labNodes gr1) `Set.isSubsetOf` (Set.fromList $ labNodes gr2)) &&
  ((Set.fromList $ labEdges gr1) `Set.isSubsetOf` (Set.fromList $ labEdges gr2))
  
subCFG :: (Ord n, Ord e) => CFG n e -> CFG n e -> Bool
subCFG cfg1 cfg2 = 
  ((graph cfg1) `subgraph` (graph cfg2)) &&
  ((entryNode cfg1) == (entryNode cfg2)) &&
  ((Set.fromList $ exitNodes cfg1) `Set.isSubsetOf` (Set.fromList $ exitNodes cfg2))
  
test_addGraph1 = 
  build $
  do n1 <- newNode 1
     setEntryNode n1
     n2 <- newNode 2
     setExitNodes [n2]
     (n3, n4) <- addGraph test_tree
     newEdge () n1 n3
     newEdges (zip (inflist ()) n4) n2
     
test_tree :: GraphBuilder Int () ()
test_tree = do n3 <- newNode 3
               n4 <- newNode 4
               n5 <- newNode 5
               newEdge () n3 n4
               newEdge () n3 n5
               setEntryNode n3
               setExitNodes [n4, n5]
               return ()
     
     
     