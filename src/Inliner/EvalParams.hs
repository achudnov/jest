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


{-# LANGUAGE FlexibleContexts #-}

module Inliner.EvalParams (EvalParams(..)
                          ,parseEvalParams) where

import Text.Parsec
import Control.Monad.Identity
import Data.Default.Class
import Data.Maybe (isJust)

data EvalParams = EvalParams {mVarName           :: String
                             ,disallowVarDecls   :: Bool}

data MyState = MyState {_mVarName :: Maybe String
                       ,_disallowVarDecls :: Maybe Bool}

instance Default MyState where
  def = MyState {_mVarName = Nothing
                ,_disallowVarDecls = Nothing}

parseEvalParams :: Stream s Identity Char => s -> Either ParseError EvalParams
parseEvalParams = runParser evalParams def ""

-- | Parser for the X-Inline header params
evalParams :: Stream s Identity Char => Parsec s MyState EvalParams
evalParams =
  do spaces
     evalParam `sepBy` (char ';')
     optional (char ';')
     s <- getState
     unless (isJust (_mVarName s) && isJust (_disallowVarDecls s)) $
       unexpected "One or more of the required parameters are missing"
     return $ state2params s

state2params :: MyState -> EvalParams
state2params (MyState (Just s) (Just b)) = EvalParams s b

evalParam :: Stream s Identity Char => Parsec s MyState ()
evalParam =
  choice [do string "monitorvar="
             s <- getState
             when (isJust $ _mVarName s) $
               unexpected "Duplicate declaration of monitorvar"
             mvar <- many alphaNum
             spaces
             modifyState $ \s -> s{_mVarName = Just mvar}
         ,do string "novardecls="
             s <- getState
             when (isJust $ _disallowVarDecls s) $
               unexpected "Duplicate declaration of novardecls"
             b <- bool
             spaces
             modifyState $ \s -> s{_disallowVarDecls = Just b}
         ] <?> "Unrecognized X-Inline parameter"
  where bool :: Stream s Identity Char => Parsec s u Bool
        bool = choice [choice (map string ["true", "1"]) >> return True
                      ,choice (map string ["false", "0"]) >> return False]
