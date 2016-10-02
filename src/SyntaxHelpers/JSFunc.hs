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


{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | An abstraction for JS functions

module SyntaxHelpers.JSFunc where

import Language.ECMAScript5.Syntax
import Data.Default.Class
import Data.Data (Data)
import Data.Typeable (Typeable)
import Data.Functor (Functor)
import Data.Traversable (Traversable)
import Data.Foldable (Foldable)
import Language.ECMAScript5.Syntax.Annotations

data JSFunc a = JSFunc [String] [Statement a]
              deriving (Data, Typeable, Functor, Foldable, Traversable)

compile :: Default a => JSFunc b -> Expression a
compile (JSFunc params body) = FuncExpr def Nothing (map (Id def) params) (map (reannotate (const def)) body)

call :: Default a => JSFunc a -> [Expression a] -> Expression a
call func args = CallExpr def (compile func) args

