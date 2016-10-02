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


module Inliner.Error (InlinerError(..)
                     ,RewritingError) where

import Text.Parsec.Error
import Control.Monad.Error

data InlinerError = ParseError ParseError
                  | RewritingError RewritingError
                  | GenericError

instance Error InlinerError where
  noMsg = GenericError
  
instance Show InlinerError where
  show (ParseError err) = "Error while parsing: " ++ show err
  show (RewritingError err) = "Error in the transformation pipeline: " ++
                              show err
  show GenericError = "Unknown error"

type RewritingError = String

