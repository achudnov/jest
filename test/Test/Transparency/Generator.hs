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

module Test.Transparency.Generator where

import Language.ECMAScript5.Syntax
import Language.Haskell.TH.Syntax


getName c = case c of
  NormalC name _ -> name
  RecC name _ -> name
  InfixC _ name _ -> name
  ForallC _ _ con2 -> getName con2

enumOps :: Name -> Q Exp
enumOps name =
  let f n = TupE [LitE $ StringL $ nameBase n, ConE n] in
  do (TyConI (DataD _ _ _ cons _)) <- reify name
     return $ ListE $ map (f . getName) cons
