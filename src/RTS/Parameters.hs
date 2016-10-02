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

module RTS.Parameters (StopFunction(..)
                      ,Environment(..)
                      ,Params(..)
                      ,RTSOpt(..)) where

import Data.Default.Class
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Data (Data)
import Data.Typeable (Typeable)
import RTS.Policy.Syntax (Policy)
import Language.ECMAScript5.Syntax

-- | Which stop function to use?
data StopFunction = Loop -- ^ the stop function will enter an infinite
                         -- loop, for production use
                  | Report -- ^ the stop function will raise an alert
                           -- with the information about the violation
                  | Exception -- ^ the stop function will throw an
                              -- exception; as of now it's not secure
                  deriving (Data, Show, Typeable)
                           
instance Default StopFunction where
  def = Loop

-- | Run-time environment of the program: which API's should be wrapped
data Environment = Standalone -- ^ Just the ECMAScript standard
                              -- library (standalone environment)
                 | Browser -- ^ ES + DOM API (browser environment)
                 | Android -- ^ ES + DOM API + Android object (Android WebView)
                 | Custom Environment {- ^ Base environment-}
                          (Program ()) {- Extra JavaScript to be appended to the RTS initialization code -}
                   
                 deriving (Data, Show, Typeable, Ord, Eq)
                        
data Params = Params {stopfn :: StopFunction 
                      -- ^ The stop function to use
                     ,policy :: Policy -- ^ What's the policy
                     ,environment :: Environment 
                      -- ^ The run-time environment to consider
                     ,rtsOptimization :: RTSOpt -- ^ the RTS optimization level
                     }

data RTSOpt = Transparent | Opaque

instance Default Params where
  def = Params {stopfn = def
               ,policy = def
               ,environment = Standalone
               ,rtsOptimization = Transparent
               }
