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


module Main where

import Language.ECMAScript5.Parser
import Language.ECMAScript5.Syntax
import Language.ECMAScript5.Syntax.Annotations
import Language.ECMAScript5.PrettyPrint
import System.IO
import System.Process
import Data.Data (Data)
import Data.Typeable (Typeable)
import Inliner.Monad
--import Inliner.Annotations
import Inliner.Desugaring
import Inliner.Analysis
import Control.Monad
import System.Environment
import System.Exit
import Data.Default.Class

main :: IO ()
main = do args <- getArgs
          when (length args == 0) exitFailure
          let (fname:_) = args
          js <- parseFromFile fname
          putStrLn $ annot js

annot :: (Typeable a, Show a, Data a, Default a) => Program a -> String
annot = showAnnotations . renderPretty 0.4 80 . prettyPrint . reannotate snd . analyse . desugar
