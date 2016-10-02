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
module RTS.Level.BitVector where

import Language.Haskell.TH.Lift
import SyntaxHelpers
import Language.ECMAScript5.Syntax
import Data.Default.Class

bitVectorImpl :: Default a => Program a
bitVectorImpl = redef $ $(loadJS "rts/level/bitvector.js")
