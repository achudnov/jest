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
{-# LANGUAGE QuasiQuotes #-}

-- | Test transparency of instrumentations (as well as the RTS). Run
-- the original program unmodified, record the outputs. Then run the
-- inliner with a trivial policy and record the outputs as well. They
-- should be equal.
module Test.Transparency (tests_transparency) where

import Test.Tasty
import Test.Common
import Test.Transparency.Randomized
import Test.Transparency.Unit


import Test.Executable
import Test.JSQuickCheck
import Language.ECMAScript5.Syntax
import Language.ECMAScript5.Syntax.Annotations
import Language.ECMAScript5.Syntax.QuasiQuote
import Control.Monad
import RTS hiding (environment)
import Inliner
import Inliner.Instrumentation
import Inliner.Monad hiding (environment)
import qualified Inliner.Monad as IM (environment)
import Data.Default.Class
import Test.TaRTuFfe

tests_transparency :: IO TestTree
tests_transparency =
  liftM (testGroup "QuickCheck executable transparency tests")$
  sequence [test_transparency_unit]
