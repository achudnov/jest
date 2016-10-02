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


{-# LANGUAGE ScopedTypeVariables #-}
-- | Implements information flow monitor inlining

module Inliner (inlineSource
               ,inlineHTML
               ,inlineEval
               ,runInliner
               ,RTS.addRTS
               ,prettyProgram
               ,withParsedSource) where

import Control.Applicative
import Control.Arrow
import Control.Arrow.ArrowIO
import Control.Arrow.ArrowList
import Control.Monad.Identity hiding (when)
import Control.Monad.Trans
import Control.Monad.Except hiding (when)
import qualified Control.Monad as M (when)

import           Data.Char
import           Data.Data
import           Data.Default.Class
import           Data.Maybe   (fromMaybe, maybeToList)
import           Data.Set     (Set)
import qualified Data.Set     as Set

import Data.Traversable
import Data.Generics.Uniplate.Data

import System.Random

import Language.ECMAScript5.Parser
import Language.ECMAScript5.Syntax
import Language.ECMAScript5.PrettyPrint
import Text.Html.Consolidate

import Text.XML.HXT.Core hiding (swap)
import Text.XML.HXT.Arrow.ReadDocument
import Text.XML.HXT.Arrow.XmlArrow
import Text.XML.HXT.Arrow.XmlState.RunIOStateArrow
import Text.XML.HXT.Arrow.XmlState.SystemConfig

import RTS.Policy
import Inliner.Error
import Inliner.Monad
import Inliner.Instrumentation
import Inliner.Annotations
import qualified RTS
import MyPrelude
import Network.URI
import Optimizations
import SyntaxHelpers
import Data.Monoid

withParsedSource :: (Program (SourceSpan, [Comment]) -> Inliner (Program ((SourceSpan, [Comment]), Annotation))) ->
                    String ->
                    Inliner String
withParsedSource f src = parseProgram src >>= f >>= return . prettyProgram False

-- | Performs complete inlining on the entire JavaScript program
inlineSource :: (Data a, Default a, Show a, Ord a, Monoid a)
             => RTS.Params -- ^ monitor RTS parameters
             -> Maybe String -- ^ explicitly specified monitor namespace prefix
             -> Program a -- ^ JavaScript program
             -> Inliner (Program (a, Annotation))
inlineSource pp mprefix js =
  do -- generate the monitor namespace prefix, if one hasn't been provided already
     prefix <- liftM (`fromMaybe` mprefix) $ generatePrefix js
     let params = RewritingParams {monitorPrefix = prefix
                                  ,rewritingMode  = Full
                                  ,environment = RTS.environment pp}
     -- transform the JavaScript source
     tjs <- instrument params js
     -- optimize, add the run-time system and pretty-print the whole program
     return $ RTS.addRTS pp prefix $ optimizeJs tjs
  
-- | A helper function for inlining on HTML (invokes HTML normalization)
inlineHTML :: RTS.Params -- ^ monitor RTS parameters
           -> String -- ^ HTML source with JavaScript
           -> Maybe URI -- ^ the URI of the page
           -> Inliner String
inlineHTML pp html mbase_uri =
  let ns = initialConsState True mbase_uri []
      --parseExtractArr = parseHTML html mbase_uri >>> (getPolicyArr &&& extractJSArr)
      parseExtractArr = parseHTML html mbase_uri >>> extractJSArr
      xioState = initialState ns in
  do --[(mpolicy, (ts, js))] <- lift $ runXIOState xioState $ single parseExtractArr
     [(ts, js)] <- lift $ runXIOState xioState $ single parseExtractArr  
     ijs <- inlineSource pp Nothing js
     lift $ renderHTML ns $ arr (const (ts, ijs)) >>> insertJSArr

-- -- | If a policy is linked from the document it downloads it, parses
-- -- and returns if all of the above was successful
-- getPolicyArr :: IOSArrow XmlTree (Maybe Policy)
-- getPolicyArr = undefined

-- -- | Searches for a <link rel="policy" href="..."> tag, removes it and
-- -- returns the value of the href attribute
-- getPolicyURI :: IOStateArrow (Maybe URI) XmlTree (Maybe URI)
-- getPolicyURI = processTopDown $ extractLinkRelPolicy `when` isLinkRelPolicy

-- isLinkRelPolicy :: ArrowXml ar => ar XmlTree XmlTree
-- isLinkRelPolicy = isElem >>> hasName "link" >>> hasAttr "rel" >>> hasAttrValue "rel" (== "policy")

-- extractLinkRelPolicy :: IOStateArrow (Maybe URI) XmlTree XmlTree
-- extractLinkRelPolicy = undefined

-- | Performs inlining of an @eval@ body
inlineEval :: (Data a, Default a, Show a, Ord a)
           => String -- ^ The monitor variable name
           -> Bool -- ^ Whether to prohibit variable declarations in the code
           -> Program a -- ^ JavaScript program, from the @eval@ param
           -> Inliner (Program (a, Annotation))
inlineEval prefix noVarDecls js = do
  M.when (noVarDecls && hasVarDecls js) $
    throwError $ RewritingError $
    "Variable declarations are prohibited, but present in the program"
  let params = RewritingParams {monitorPrefix = prefix
                               ,rewritingMode  = Eval}
  tjs <- instrument params js
  return $ optimizeJs tjs
 where hasVarDecls :: forall a. Data a => Program a -> Bool
       hasVarDecls js =
         not $ null $ [v | v@(VarDeclStmt _ _) <- universeBi js :: [Statement a]]

inlineHTMLEval :: String -- ^ The monitor variable name
               -> String -- ^ HTML source with JavaScript
               -> Maybe URI -- ^ the URI of the page that the fragment
                            -- is going to be inserted
               -> Inliner String
inlineHTMLEval = undefined

-- | Collects all the declared and referenced variable names (and
-- throws some standard ones in the mix) and generates a fresh unused
-- variable name
generatePrefix :: (Data a) => Program a -> Inliner String
generatePrefix js =
  let vars = mkPrefixTree $ Set.toList $ boundVariables js
      randomJSVar = do l <- randomLetter
                       rest <- replicateM 7 randomLetterOrDigit
                       return (l:rest)
      randomLetter = do ri <- randomRIO (0,25)
                        rc <- randomRIO (0::Int, 1)
                        let base = if rc == 1 then 65 {-A-} else 97 {-a-}
                        return $ chr (ri+base)
      randomLetterOrDigit = randomRIO (0,61) >>= \ri ->
        return $ chr $ case ri of
          _ | ri < 26 -> 65 + ri {-uppercase letter-}
          _ | ri >= 26 && ri < 52 -> 97 + ri - 26 {-lowercase letter-}
          _ | otherwise -> 48 + ri - 52 {- digit -}
      genMonVarName = do jsvar <- randomJSVar
                         if jsvar `isPrefixOf` vars then genMonVarName
                                                    else return jsvar
  in lift genMonVarName

-- | A simple wrapper for
-- 'Language.ECMAScript5.Parser.parseScriptFromString' that converts
-- 'ParseError' to 'String'
parseProgram  :: String -> Inliner (Program (SourceSpan, [Comment]))
parseProgram prg = case parseFromString prg of
  Left err -> throwError $ ParseError err
  Right js -> return js

-- | Pretty prints JavaScript AST
prettyProgram :: Show a => Bool -> Program a -> String
prettyProgram printAnnots =
  let fdisplay = if printAnnots then showAnnotations else display
  in  fdisplay . renderPretty 0.4 80 . prettyPrint
