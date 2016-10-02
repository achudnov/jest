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


{-# LANGUAGE TupleSections #-}
module Test.CFG.ControlFlow where

import CFG.Common
import CFG.ControlFlow
import CFG.Construct
import Test.Tasty
import Test.Tasty.QuickCheck
import Language.ECMAScript5.Syntax
import Language.ECMAScript5.Syntax.CodeGen
import Language.ECMAScript5.Syntax.Arbitrary
import Language.ECMAScript5.Syntax.Annotations
import Language.ECMAScript5.PrettyPrint
import Lens.Simple
--import Inliner.Annotations
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M
import Data.Default.Class
import Data.Default.Instances.Base
import SyntaxHelpers
import Control.Applicative
import Data.Traversable
import Control.Arrow
import Data.Graph.Inductive.Graph hiding (prettyPrint)
import Data.Maybe (fromJust)

testControlFlow = testProperty "Control flow analysis invariants"
                  prop_CFAInvar

-- (1) Push can only be on exit nodes of expressions
-- (2) Pop can be on either the exit nodes of expressions, or entry and exit nodes of statements
-- (3) Can have at most one push or pop

prop_CFAInvar :: Program () -> Property
prop_CFAInvar js = conjoin $ map snd $ M.toList $ M.mapWithKey check $ graphAnalysis $ cfg
  where -- A node can either have a Push or a Pop annotation, but not both
        -- atMostOnePushOrPop :: Node -> [PushPop] -> Bool
        -- atMostOnePushOrPop _ = (<= 1) . length

        -- A node can have at most one of each Push
        atMostOnePush _ = (<= 1) . foldl (\acc pp -> case pp of
                                             Push _ -> acc+1
                                             _      -> acc) 0
          -- uncurry (&&) . ((<=1) *** (<=1))
                         -- . foldl (\acc pp -> case pp of
                         --               Push' _ -> first (+1) acc
                         --               Pop     -> second (+1) acc) (0, 0)
        -- Push can only be on end nodes of expressions
        pushOnlyOnExitNodesExpr n [Push _] = case nodeLab n of
          End _ -> True
          _     -> False
        pushOnlyOnExitNodesExpr _ _ = True
        -- Pop can be on either the exit nodes of expressions, or
        -- entry and exit nodes of statements
        popNotOnFakeNodes n = and . map (popNotOnFakeNode n)
        popNotOnFakeNode n Pop = case nodeLab n of
          NormalReturn -> False
          ExceptionalReturn -> False
          Ghost -> False
          _ -> True
        popNotOnFakeNode _ _ = True
        debugInfo n pp = show (prettyPrint js) ++ " // " ++ show n ++ ": " ++ show pp
        check :: Node -> [PushPop] -> Property
        check n pp = counterexample (debugInfo n pp) $ and $ sequence (map ($ n) [atMostOnePush, pushOnlyOnExitNodesExpr, popNotOnFakeNodes]) pp
        cfg = snd $ constructCFG ss
        ss :: [Statement (CFAAnnot ())]
        ss = unProgram $ redef js 
        nodeLab = fromJust . lab (cfg^.graph)
