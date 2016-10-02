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


module TransformationConsole where

import Inliner.Instrumentation
import Inliner.Analysis
import Inliner.Annotations
import Inliner.Monad
import Language.ECMAScript5.Syntax
import Language.ECMAScript5.Syntax.Annotations
import Data.Traversable
import Language.ECMAScript5.Parser
import Language.ECMAScript5.PrettyPrint
import Data.Data (Data)
import Data.Default.Class
import Control.Applicative
import Control.Monad
import Text.Parsec.Pos (initialPos)
import CFG.Dot
import System.Process
import System.IO
import Data.Data (Data)
import Data.Typeable (Typeable)
import CFG.Construct
import CFG.Common

-- | Runs a rewriting pass on a piece of JavaScript code
run :: (Default a, Show a)
    => (a -> Rewriter b) -> a -> IO b
run t a = do
  let params = RewritingParams {monitorPrefix = "monitor"
                               ,rewritingMode = Full}
  res <-runInliner $ rewriter2inliner params $ t a
  case res of
    Left err -> error $ "Inlining failed: " ++ show err
    Right js -> return js

run_jst :: forall a. (Ord a, Data a, Default a, Show a)
        => Program a -> IO (Program (a, Annotation))
run_jst js = do
  let params = RewritingParams {monitorPrefix = "monitor"
                               ,rewritingMode = Full}
  res <-runInliner $ instrument params js
  case res of
    Left err -> error $ "Inlining failed: " ++ show err
    Right js -> return js

projectTA :: Traversable x => x (a, Annotation) -> x Annotation
projectTA = reannotate snd

run_jstp :: (Data a, Ord a, Show a, Default a) => Program a -> IO ()
run_jstp js = ((pretty . projectTA) <$> run_jst js) >>= putStrLn

-- | Pretty prints JavaScript with annotations
pretty :: (Show a, Pretty (p a) a) => p a -> String
pretty = showAnnotations . renderPretty 0.2 80 . prettyPrint

-- | Parses JavaScript from string, runs a rewriting pass and
-- pretty prints it
parseAndRun :: (Program ((SourceSpan, [Comment]), Annotation) -> 
                Rewriter (Program ((SourceSpan, [Comment]), Annotation)))
            -> String -> IO String
parseAndRun t s = case parseFromString s of
  Left err -> error $ "Parse failed: " ++ show err
  Right js -> runAndPretty t js

runAndPretty :: forall a. (Ord a, Data a, Default a, Show a)
             => (Program (a, Annotation) -> 
                 Rewriter (Program (a, Annotation)))
             -> Program a -> IO String
runAndPretty t js = pretty <$> run t (initAnnotations js)

-- | Runs a rewriting pass on a piece of JavaScript code
runE :: forall a. (Ord a, Data a, Default a, Show a)
    => (Expression (a, Annotation) -> 
        Rewriter (Expression (a, Annotation)))
    -> Expression a -> IO (Expression (a, Annotation))
runE t e = do
  let params = RewritingParams {monitorPrefix = "monitor"
                               ,rewritingMode = Full}
  res <- runInliner $ rewriter2inliner params $ (initAnnotationsE >=> t) e
  case res of
    Left err -> error $ "Inlining failed: " ++ show err
    Right e -> return e

initAnnotationsE = return . reannotate (\a -> (a, def))

-- | Parses Expression from string, runs a rewriting pass and
-- pretty prints it
parseAndRunE :: (Expression ((SourceSpan, [Comment]), Annotation) -> 
                 Rewriter (Expression ((SourceSpan, [Comment]), Annotation)))
             -> String -> IO String
parseAndRunE t s = case parse expression "" s of
  Left err -> error $ "Parse failed: " ++ show err
  Right e -> runAndPrettyE t e

runAndPrettyE :: forall a. (Ord a, Data a, Default a, Show a)
             => (Expression (a, Annotation) -> 
                 Rewriter (Expression (a, Annotation)))
             -> Expression a -> IO String
runAndPrettyE t e = pretty <$> runE t e

graph :: (Typeable a, Data a) => FilePath -> Program a -> IO ()
graph f js = do
  let dot = showCFG $ snd $ constructCFG $ unProgram $ reannotate liftCFAAnnot $ labelSets $ nativeErrorAnalysis $ initAnnotations js
  (dotin, dotout, _, pid) <- runInteractiveProcess "dot" ["-Tsvg"] Nothing Nothing
  hPutStr dotin dot
  img <- hGetContents dotout
  writeFile f img
