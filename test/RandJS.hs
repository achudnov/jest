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

module Main where

import System.Console.CmdArgs.Implicit
import Language.ECMAScript5.Syntax (Program)
import Language.ECMAScript5.Syntax.Arbitrary ()
import Language.ECMAScript5.PrettyPrint
import Test.QuickCheck (sample', resize, arbitrary, Gen)

data Options = Options {sampleSize :: Int}
             deriving (Data, Typeable, Show)

main :: IO ()
main = do o <- cmdArgs options
          (js:_) <- sample' (resize (sampleSize o) arbitrary :: Gen (Program ()))
          putStrLn $ show $ prettyPrint js

options :: Options
options = Options {sampleSize = 5 &= help "Sample size" &= opt "5"}
