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


-- | An EDSL for transformations on the JavaScript Abstract Syntax trees
module SyntaxHelpers.ASTTransformations (beforeS,
                                         afterS,
                                         beforeE,
                                         afterE) where

import Language.ECMAScript5.Syntax
import Language.ECMAScript5.Syntax.CodeGen (block, list)
import SyntaxHelpers.JSFunc
import Data.Default.Class

beforeS :: Default a => Statement a -> Statement a -> Statement a
beforeS x s = block [x, s]

afterS :: Default a => Statement a -> Statement a -> Statement a
afterS x s = block [s, x]

beforeE :: Default a => Expression a -> Expression a -> Expression a
beforeE x e = list [x, e]

afterE :: Default a => JSFunc a -> Expression a -> Expression a
afterE f e = call f [e]
