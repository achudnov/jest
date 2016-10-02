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
module RTS.Policy.Syntax where

import Language.ECMAScript5.Syntax
import Network.URI
import Text.Regex.TDFA
import Data.List
import Control.Applicative
import Data.Default.Class
import Data.Data (Data)
import Data.Typeable (Typeable)
import RTS.Level
import Data.Set (Set)
import Data.Map (Map)

type PolicyDB = [(URITemplate, Policy)]
type URITemplate = String

data Policy = Policy LevelImplementation ChannelMap
              deriving Show

instance Default Policy where
  def = Policy BitVector ChannelMapTrivial

data ChannelMap = ChannelMapFunction (Expression ())
                | ChannelMapPrincipals (Map (Channel, ReadWrite) (Set Label))
                | ChannelMapTrivial
                  deriving Show

data Label = NoLabel
           | Label String
           deriving (Ord, Eq, Show)

data ReadWrite = Read | Write
               deriving (Show, Ord, Eq)
                   
data Channel = Network URITemplate
             | Cookie  Domain
             | Input   ElementSelector
             | Function FunctionName (Maybe Int)
               deriving (Show, Ord, Eq)
               
data ElementSelector = ByElementId String
                     deriving (Show, Ord, Eq)

type FunctionName = String

type Domain = String

policyForURI :: PolicyDB -> URI -> Maybe Policy
policyForURI pdb uri =
  let uri_string = show uri
  in  snd <$> find (\(pat, _) -> uri_string =~ pat) pdb
