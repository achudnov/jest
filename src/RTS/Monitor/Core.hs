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
{-# LANGUAGE FlexibleContexts #-}

-- | Definition of monitor constructor (JavaScript code) and bindings to monitor operations
module RTS.Monitor.Core where

import Language.ECMAScript5.Syntax
import Language.ECMAScript5.Syntax.CodeGen
import Data.Default.Class
import RTS.Monitor.APISpec
import Control.Applicative
import Control.Monad.Reader

loadAndParse "rts/core.js" >>= \js -> mkBindings js "monitorCore"

monitorComponentRefM :: (Default a, MonitorPrefix p, MonadReader p m) => Id a -> m (Expression a)
monitorComponentRefM (Id _ name)= ask >>= (return . var . ident . (++ name) . getPrefix)
