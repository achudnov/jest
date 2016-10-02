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


{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | some simple optimizations on JavaScript ASTs
module Optimizations where

import Language.ECMAScript5.Syntax
import Data.Generics.Uniplate.Data
import Data.Data (Data)

-- | optimizes constructs like {s1; {s2;...}}
optimizeBlocks :: forall a. Data a => Program a -> Program a
optimizeBlocks = transformBi optimize
  where optimize :: [Statement a] -> [Statement a]
        optimize = foldr flattenBlock []
        flattenBlock (BlockStmt a ss) acc = ss++acc
        flattenBlock s acc = s:acc
        
-- | simple constant folding        
constantFold :: forall a. Data a => Program a -> Program a 
constantFold = transformBi cfold
  where cfold :: Expression a -> Expression a
        cfold = id -- not implemented

-- | removes empty statements
rmEmpty :: forall a. Data a => Program a -> Program a
rmEmpty = transformBi removeEmpty
          where removeEmpty :: [Statement a] -> [Statement a]
                removeEmpty = filter isNotEmpty
                isNotEmpty s = case s of
                  EmptyStmt _ -> False
                  _           -> True

-- | runs all the available optimizations
optimizeJs :: Data a => Program a -> Program a
optimizeJs = optimizeBlocks . constantFold . rmEmpty
