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


module Inliner.Instrumentation where

import Data.Data (Data)
import Data.Default.Class
import Language.ECMAScript5.Syntax
import Inliner.Annotations
import Inliner.Monad
import Inliner.Desugaring
import Inliner.Analysis
import Inliner.Rewriting

-- | Top level instrumentation pipeline
instrument :: (Ord a, Data a, Default a, Show a)
           => RewritingParams
           -> Program a
           -> Inliner (Program (a, Annotation))
instrument tp = rewriter2inliner tp . rewrite . analyse . desugar
