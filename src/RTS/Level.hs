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


{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module RTS.Level where

import Data.Default.Class
import SyntaxHelpers
import RTS.Level.BitVector
import RTS.Level.StringSet
import Language.ECMAScript5.Syntax
import Language.ECMAScript5.Syntax.CodeGen
import Data.Data (Data)
import Data.Typeable (Typeable)

data LevelImplementation = BitVector | StringSet | Custom (Expression ())
                         deriving (Data, Show, Typeable)

instance Eq LevelImplementation where
  BitVector == BitVector = True
  StringSet == StringSet = True
  Custom e1 == Custom e2 = e1 == e2
  _ == _ = False
  

compileLevelImpl :: Default a => LevelImplementation -> [Statement a]
compileLevelImpl li = case li of
  BitVector  -> unProgram bitVectorImpl
  StringSet  -> unProgram stringSetImpl
  Custom obj -> [vardecls [varinit internalLevelCtorName $ redef obj]]

internalLevelCtorName :: Default a => Id a
internalLevelCtorName = "LevelCtor"
