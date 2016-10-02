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


{-# LANGUAGE RankNTypes #-}

module Inliner.Monad (Inliner
                     ,Rewriter
                     ,runInliner
                     ,rewriter2inliner
                     ,genUID
                     ,isEvalMode
                     ,evalModeOnly
                     ,RewritingParams(..)
                     ,RewritingMode(..)
                     ) where

import Control.Monad.Reader -- mtl
import Control.Monad.Except -- mtl
import Control.Monad.State --mtl
import Control.Monad (liftM)
import Data.Default.Class
import Inliner.Error
import Language.ECMAScript5.Syntax (Id(..))
import RTS.Parameters (Environment(..))
import RTS.Monitor.APISpec (MonitorPrefix (..))

data RewritingMode =
    Full -- ^ Full rewriting mode, for page or standalone code
  | Eval -- ^ Rewriting mode for code passed to the @eval@
         -- function.

instance MonitorPrefix RewritingParams where
  getPrefix = monitorPrefix

data RewritingParams = 
  RewritingParams {
    rewritingMode :: RewritingMode
   ,monitorPrefix :: String -- ^ the prefix to monitor operations/state names
   ,environment :: Environment
   }

instance Default (RewritingParams) where
  def = RewritingParams {rewritingMode = Full
                        ,monitorPrefix = "monitor"
                        ,environment = Browser}

-- | The inliner monad: can fail and can perform IO
type Inliner = ExceptT InlinerError IO

runInliner :: Inliner r -> IO (Either InlinerError r)
runInliner = runExceptT

-- | The transformer monad: has an immutable state
-- ('RewritingParams') mutable state (a counter for generating
-- unique id's for remember/restore) and can fail (wraps around the
-- InlinerT transformer. NOTE: it might be a better idea to use a
-- counter monad instead of a state monad
type Rewriter r =
    StateT Int (ReaderT RewritingParams Inliner) r
  
-- | Removes the two outer monads from the transformer, so it could
-- run in an inliner monad
rewriter2inliner :: RewritingParams -- ^ rewriting parameters
                 -> Rewriter r -- ^ the transformer to run
                 -> Inliner r
rewriter2inliner params transformer = 
  (`runReaderT` params) $ (`evalStateT` 1) transformer
                                    
-- | returns the next number of the counter (used for remember/restore)
genUID :: Rewriter Int
genUID = get >>= (\i -> put (i+1) >> return i)

-- returns whether we're in the eval mode
isEvalMode :: Rewriter Bool
isEvalMode = ask >>= \ps -> return $ case rewritingMode ps of
  Eval  -> True 
  _     -> False

-- executes the parameter only if we're in the eval mode
evalModeOnly :: (a -> Rewriter a) -> a -> Rewriter a
evalModeOnly t x = isEvalMode >>= \em ->
  if em then t x else return x

getEnvironment :: Rewriter Environment
getEnvironment = liftM environment ask
