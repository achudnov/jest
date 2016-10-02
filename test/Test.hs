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


-- | main entry point for tests
module Main where

import Test.Tasty
import Test.CFG
import Test.Rewriting
import Test.RTS
import Test.Transparency
import Test.Desugaring
          
-- entry point for the test-suite
main = tests >>= (defaultMain . testGroup "Tests")

tests :: IO ([TestTree])
tests = sequence [return tests_rts
                 ,return tests_CFG
                 ,test_rewriting
                 ,tests_transparency
                 ,return tests_desugaring
                 -- ,tests_security
                 ]
