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


module Main where

import Language.ECMAScript5.Parser
import Language.ECMAScript5.Syntax
import Language.ECMAScript5.Syntax.Annotations
import CFG.Dot
import System.IO
import System.Process
import Data.Data (Data)
import Data.Typeable (Typeable)
import Inliner.Instrumentation
import Inliner.Monad
import Control.Monad
import Inliner.Analysis
import System.Exit
import System.Environment
import CFG.Construct
import CFG.Common
import CFG.ControlFlow
import Data.Tree
import Data.Tree.Pretty
import System.EasyFile
import Data.List
import Lens.Simple
import Data.IntMap (IntMap)
import qualified Data.IntMap as M
import Data.Graph.Inductive.Graph hiding ((&))
import Data.Graph.Inductive hiding (NodeMap)
import Data.Graph.Inductive.Dot


main :: IO ()
main = do args <- getArgs
          when (length args == 0) exitFailure
          let (fname:_) = args
          (dot, pdt) <- liftM printGraph (parseFromFile fname)
          writeFile (replaceExtension fname "dot") dot
          writeFile (replaceExtension fname "pdt") pdt
          

printGraph :: (Typeable a, Data a) => Program a -> (String, String)
printGraph js =
  let gr = annotateWithGraphAnalysis cfg
      cfg = snd
            $ constructCFG
            $ unProgram
            $ reannotate liftCFAAnnot
            $ labelSets
            $ nativeErrorAnalysis
            $ initAnnotations js
  in (showDot $ dotGr gr, drawVerticalForest $ toForest $ postDominatorTree cfg)

annotateWithGraphAnalysis :: CFG -> Gr (NodeLabel, [PushPop]) ()
annotateWithGraphAnalysis cfg =
  let cfaMap = graphAnalysis cfg
  in  gmap (\(ins, n, lab, outs) -> (ins, n, (lab, joinMaybeMonoid $ M.lookup n cfaMap), outs)) $ cfg^.graph

toForest :: (NodeMap, Int) -> Forest String
toForest = subForest . toTree

toTree :: (NodeMap, Int) -> Tree String
toTree (nm, root) =
  unfoldTree (\n -> (show n, joinMaybeMonoid $ M.lookup n revTree)) root
  where revTree :: IntMap [Int]
        revTree = M.fromListWith (++) $ map (\(a,b) -> (b, [a])) (M.toList nm)

joinMaybeMonoid :: Monoid a => Maybe a -> a
joinMaybeMonoid Nothing  = mempty
joinMaybeMonoid (Just a) = a

