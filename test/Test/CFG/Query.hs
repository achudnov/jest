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


module Test.CFG.Query where

import CFG.Common
import CFG.Query
import CFG.Builder
import Test.Common

test_successors cfg n = 
  graphQuery q cfg 
  where q = do setFinger n
               successors
               
test_predecessors cfg n = 
  graphQuery q cfg 
  where q = do setFinger n
               predecessors
               
inspect_context cfg n =
  graphQuery q cfg
  where q = do setFinger n
               currentContext
               
prop_setGetFinger cfg n =
  let q = setFinger n >> getFinger in
  case graphQuery q cfg of
    Right fin -> fin == n
    Left _    -> True
  
setGetFinger cfg n =
  let q = setFinger n >> getFinger in
  runQuery q cfg 1
                  
test_return cfg = let q = return 3 >> return 4 in
  runQuery q cfg 1