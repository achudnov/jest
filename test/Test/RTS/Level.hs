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


{-# LANGUAGE QuasiQuotes #-}

module Test.RTS.Level (test_levels) where

import Inliner

import Test.Tasty
import Control.Monad

import Language.ECMAScript5.Parser
import Language.ECMAScript5.Syntax
import Language.ECMAScript5.Syntax.QuasiQuote
import Language.ECMAScript5.Syntax.Annotations
import Language.ECMAScript5.PrettyPrint

import qualified Data.Set as Set

import Test.Common
import Test.Executable
import Test.RTS.Common
import Test.JSQuickCheck hiding (environment)
import Data.Default.Class
import Control.Arrow
import RTS.Level

import Data.Traversable

import RTS


test_levels :: TestTree
test_levels =
  testGroup "Level implementation executable tests" $
  groupByLevelImpl test_cases

test_cases :: [JSProp]
test_cases = [JSProp "Bottom is the lowest element"
                   ["l" ::: arbLevel]
                   (udef [js|c.assert(monitorlowlevel().leq(l));|])
             ,JSProp "Join bounds the argument"
                   ["l1" ::: arbLevel, "l2" ::: arbLevel]
                   -- \forall l1,l2: arbLevel. l1.leq(l1.join(l2)) /\ l2.leq(l1.join(l2))
                   (udef [js|c.assert(l1.leq(l1.join(l2)));
      	                     c.assert(l2.leq(l1.join(l2)));|])
             ,JSProp "Join is symmetric"
                   ["l1" ::: arbLevel, "l2" ::: arbLevel]
                   -- \forall l1,l2: arbLevel. l1.join(l2)==l2.join(l1)
                   (udef [js|function eq (l1, l2) {
                                return (l1.leq(l2) && l2.leq(l1));
                             }
                             c.assert(eq(l1.join(l2), l2.join(l1)));|])
             ,JSProp "leq is reflexive"
                   ["l" ::: arbLevel]
                   -- \forall l1: arbLevel. l.leq(l)
                   (udef [js|c.assert(l.leq(l));|])
             ,JSProp "Join is stable (l.join(l) === l)"
              ["l" ::: arbLevel]
              (udef [js|function eq (l1, l2) {
                                return (l1.leq(l2) && l2.leq(l1));
                        }
                        c.assert(eq(l.join(l), l));|])
             ]
