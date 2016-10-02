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


-- | Print control-flow graphs in the dot format for visualization
module CFG.Dot (dotCFG, showCFG, dotGr) where

import CFG.Common
import Data.Graph.Inductive.Dot
import Language.ECMAScript5.Syntax
import Language.ECMAScript5.Syntax.Annotations
import Data.Graph.Inductive hiding (prettyPrint)
import Data.Generics.Uniplate.Data
import Text.Dot
import Data.Typeable (Typeable)
import Data.Data (Data)
import Data.Traversable (Traversable)
import Lens.Simple
import Inliner.Annotations

-- | Generates the dot representation of the control-flow graph
dotCFG :: CFG -> Dot ()
dotCFG cfg = fglToDotString (gmap nodeLabel $ emap (const "") $ cfg^.graph)

dotGr :: (Show a, DynGraph gr) => gr a b -> Dot ()
dotGr = fglToDotString . gmap nodeLabel . emap (const "")

nodeLabel :: (Show a) => Context a x -> Context String x
nodeLabel (in_, n, lab, out) = (in_, n, show n ++ ": " ++ show lab, out)

-- | Prints the dot representation of the control-flow graph as a string
showCFG :: CFG -> String
showCFG = showDot . dotCFG
