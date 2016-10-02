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


{-# LANGUAGE ScopedTypeVariables #-}

module CFG.ControlFlow (controlFlow
                       ,postDominatorTree
                       ,graphAnalysis
                       ,NodeMap
                       ,PushPop(..)) where

import MyPrelude
import Lens.Simple
import Language.ECMAScript5.Syntax
import Language.ECMAScript5.Syntax.Annotations
import Language.ECMAScript5.PrettyPrint
import Inliner.Annotations
import CFG.Common
import CFG.Construct
import Data.Set (Set, (\\))
import qualified Data.Set as Set
import Data.Default.Class
import Data.Generics.Uniplate.Data
import Data.Data (Data)
import Data.Typeable (Typeable)
import Control.Applicative
import Control.Monad hiding (guard)
import SyntaxHelpers
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Control.Monad.Writer hiding (guard)
import Data.Graph.Inductive hiding (NodeMap, (&), prettyPrint)
import Control.Arrow
import Data.List (partition)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M
import Data.Maybe (catMaybes, fromJust)

-- | Marks the push and pop directives. These
-- annotations tell the inliner where to insert monitor operations
-- that manipulate PCLS to track implicit dependencies at
-- run-time.
controlFlow :: (Data a, Typeable a)
            => Program (a, Annotation) -> Program (a, Annotation)
controlFlow = transformBodies controlFlowAnalysis

-- For each body (top-level script or function):
controlFlowAnalysis :: (Data a, Typeable a)
                    => [Statement (a, Annotation)] -> [Statement (a, Annotation)]
controlFlowAnalysis ss =
  -- Construct the CFG
  let (ss', cfg)   = constructCFG $ map (reannotate liftCFAAnnot) ss
      cfaMap = graphAnalysis cfg
  -- Write annotations to the AST
  in map (reannotate lowerCFAAnnot . traverseAnnotationsWithinBody (recordAnnotation cfaMap)) ss'

-- | The sub-analysis of controlFlowAnalysis that operates solely on
-- the control-flow graph
graphAnalysis :: CFG -> IntMap [PushPop]
graphAnalysis cfg = M.fromListWith (++) $ map (second $ \x->[x]) cfaMapListAdjusted
  where  -- Move the pop annotations on NormalReturn and ExceptionalReturn
         -- to the predecessors
        cfaMapListAdjusted = concatMap
          (\o@(n, p) ->
            if n == cfg^.normalReturnNode && p == Pop
            then zip normalReturnPreds (repeat Pop)
            else if n == cfg^.exceptionalReturnNode && p == Pop
                 then zip exceptReturnPreds (repeat Pop)
                 else [o]) cfaMapListNoGhost
        normalReturnPreds = pre (cfg^.graph) (cfg^.normalReturnNode)
        exceptReturnPreds = pre (cfg^.graph) (cfg^.exceptionalReturnNode)
        -- Filter out the annotations on the ghost exit
        cfaMapListNoGhost = filter ((/= ghost) . fst) cfaMapList
        -- Traverse the branching nodes, finding out their IPD in the
        -- PDT, recording the appropriate annotations in the map
        cfaMapList :: [(Node, PushPop)]
        cfaMapList = concat $ foldl
          (\ml bn -> maybe ml (\ipd ->
                                (if isSynthetic ipd then [(bn, Push Nothing)]
                                 else [(bn, Push $ Just ipd), (ipd, Pop)]):ml
                              ) $ M.lookup bn pdt
          ) [] branching
        -- Get all the list of branching nodes in the graph
        branching    = filter ((> 1) . outdeg (cfg^.graph)) $ nodes (cfg^.graph)
        -- Returns true only if the node is synthetic: Ghost or ExceptionalReturn
        isSynthetic i = case lab (cfg^.graph) i of
          Nothing -> True
          Just l  -> case l of
            ExceptionalReturn -> True
            Ghost -> True
            _     -> False
        (pdt, ghost) = postDominatorTree cfg

recordAnnotation :: (Data a, Typeable a) => IntMap [PushPop] -> CFAAnnot a -> CFAAnnot a
recordAnnotation cfaMap a = entryAnnot $ exitAnnot a
  where entry = a^.cfgEntryNode
        exit = a^.cfgExitNode
        entryAnnot :: CFAAnnot a -> CFAAnnot a
        entryAnnot a =
          case joinMaybeMonoid $ M.lookup entry cfaMap of
            [] -> a
            [Pop] -> a&inlinerAnnot.transformAnn.beforePop .~ Just (int2int32 entry)
            [Push i] -> error $ "Unexpected push " ++ show i ++ " on an entry node"
            -- ^ shouldn't have Push for an entry node, as there is no
            -- sensible 'before push' transformation!
            ans  -> error $ "unexpected multiple control-flow annotations on an entry node " ++ show ans
        exitAnnot :: CFAAnnot a -> CFAAnnot a
        exitAnnot a = foldl adaptXAnnot a $ joinMaybeMonoid $ M.lookup exit cfaMap
        adaptXAnnot a (Push mi) =
          let ann = case mi of
                Nothing -> PushException
                Just i  -> PushGuard (int2int32 i)
          in a&inlinerAnnot.transformAnn.push .~ ann
        adaptXAnnot a Pop      = a&inlinerAnnot.transformAnn.afterPop .~ Just (int2int32 exit)

data PushPop = Push (Maybe Node) -- ^ If `Nothing`, then the IPD is
                                 -- SEN or ExceptionalReturn
             | Pop
             deriving (Eq, Show)

-- | Computes the post-dominator tree; it is rooted at a "ghost exit
-- node" that it introduces.
postDominatorTree :: CFG -> (NodeMap, Node)
postDominatorTree cfg =
  -- insert a new node that ties all the sinks in one
  let (gr, ghostExit) = tieSinks cfg
      -- reverse the direction of all edges
      reversed = grev gr
      -- compute the dominator tree in the reversed graph (equivalent
      -- to the post-dominator tree in the original graph)
  in  (domTree reversed ghostExit, ghostExit)

type NodeMap = IntMap Node

-- | Inserts a new node that is connected to all the terminal edges
tieSinks :: CFG -> (Gr NodeLabel (), Node)
tieSinks cfg = 
  let ghost_id = head $ newNodes 1 $ cfg^.graph
      -- insert a ghost exit node
      gr2 = insNode (ghost_id, Ghost) $ cfg^.graph
      -- connect all the terminal nodes to the ghost exit
      gr3 = foldr (\x gr -> insEdge (x, ghost_id, ()) gr) gr2
            [cfg^.normalReturnNode, cfg^.exceptionalReturnNode]
  in (gr3, ghost_id)

domTree :: Graph gr => gr a b -> Node -> NodeMap
domTree gr start = M.fromList $ iDom gr start
