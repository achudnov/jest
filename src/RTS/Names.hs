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


{-# LANGUAGE OverloadedStrings #-}
module RTS.Names where

import Data.Default.Class
import Language.ECMAScript5.Syntax
import Language.ECMAScript5.Syntax.CodeGen

levelCtorName :: Default a => Id a
levelCtorName = "Level"
stopFnName :: Default a => Id a
stopFnName = "stop"
boxCtorName :: Default a => Id a
boxCtorName = "Box"
globalProxyName :: Default a => Id a
globalProxyName = "globalProxy"
policyVarName :: Default a => Id a
policyVarName = "policy"
