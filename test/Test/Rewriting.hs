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


-- | Syntactic unit-tests of program rewriting

module Test.Rewriting (test_rewriting) where

import Test.Tasty
import Test.Tasty.Golden.Advanced
import System.Directory
import qualified System.FilePath as FP
import System.IO hiding (utf8)
import Control.Monad
import Control.Monad.IO.Class
import Inliner
import Data.Default.Class
import Data.Algorithm.Diff
import Data.Algorithm.DiffOutput
import Control.Applicative
import Language.ECMAScript5.Parser
import Language.ECMAScript5.Syntax
import Language.ECMAScript5.Syntax.Annotations
import Language.ECMAScript5.SourceDiff
import Test.Common

test_rewriting :: IO TestTree
test_rewriting = 
               do allCases <- getDirectoryContents casesDir
                  allExpects <- getDirectoryContents expectsDir
                  let validCases = getValid allCases
                  let validExpects = getValid allExpects
                  return $ testGroup "Syntactic rewriting tests" $
                    map genTest $ filter (`elem` validExpects) validCases
                    where getValid = filter $ \x -> FP.takeExtension x == ".js"

casesDir = "test-data/syntactic/cases"
expectsDir = "test-data/syntactic/expects"

genTest :: FilePath -> TestTree
genTest test =
  let caseFileName = casesDir `FP.combine` test
      expectFileName = expectsDir `FP.combine` test
  in goldenTest test (liftIO $ parseFromFile expectFileName)
     (liftIO (parseFromFile caseFileName >>=
              (runInliner . inlineEval "monitor" False) >>=
              leftToFail >>= return . reannotate fst))
     verifyOutput
     (const $ return ())

verifyOutput :: Program a -> Program a -> IO (Maybe String)
verifyOutput actual expected =
  let react = removeAnnotations actual
      reexp = removeAnnotations expected
  in if react == reexp then return Nothing
     else return $ Just $ jsDiff react reexp
