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


{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Main where

import System.IO
import Data.Default.Class
import Language.ECMAScript5.Syntax
import Language.ECMAScript5.Syntax.Annotations
import Language.ECMAScript5.Syntax.CodeGen
import Language.ECMAScript5.Parser hiding (program)
import Language.ECMAScript5.PrettyPrint
import System.IO.Temp
import System.Process
import Control.Monad
import Text.HJson
import Text.HJson.Query
import qualified Data.Map as Map
import Control.Applicative hiding (empty)
import Data.Maybe
import System.Exit
import Inliner
import Inliner.Monad
import RTS
import Control.Arrow
import Control.Monad.Except
import SyntaxHelpers
import System.Environment

-- | The program expects two arguments: a path to the file with a
-- (line-separated) list of paths to benchmarks and the path to the
-- file where the ready-to-run code should be written (requires
-- nodejs).
main :: IO ()
main = do (inFile:outFile:rest) <- getArgs
          input <- readFile inFile
          let benchPaths = lines input
              parse s = parseFromFile s >>= (return . (s,) . removeAnnotations)
          benchSources <- mapM parse benchPaths
          let opaqueopt = "--oo" `elem` rest
          bench <- genBenchmarkCode opaqueopt benchSources
          writeFile outFile $ show $ prettyPrint bench

{- Note [Benchmark code structure]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     var Benchmark = require ('benchmark');
     var uninstrumentedSuite = new Benchmark.Suite();
     var instrumentedSuite = new Benchmark.Suite();
     /* uninstrumented benchmarks */
     /*... */
     var uninstrumentedBenches = {"access-binary-trees": function () {
                                   /* benchmark code*/
                                   }
                                 ,...
                                 };

     /* instrumented benchmarks */
     var instrumentedBenches = (function () {
       /*MRTS init */
       with (globalProxy.v) {
          /*...*/
          return {"access-binary-trees" : function () {
                   /* instrumented benchmark code */
                   }
                 ,...
                 };
       }
     })();

     for (var b in uninstrumentedBenches) {
       uninstrumentedSuite.add(b, uninstrumentedBenches[b]);
       instrumentedSuite.add(b, instrumentedBenches[b]);
     }

     uninstrumentedSuite.run();
     instrumentedSuite.run();

     var result = {};
     for (var i = 0; i < uninstrumentedSuite.length; i++) {
	 var name = uninstrumentedSuite[i].name;
       if (name !== instrumentedSuite[i].name) throw "The names of instrumented and uninstrumented benchmarks don't match!'";
       result[name] = {instrumented: instrumentedSuite[i].stats
                      ,uninstrumented: uninstrumentedSuite[i].stats};
     }

     console.log(JSON.stringify(result));
-}

-- | Generates the JavaScript source of the benchmark runner. Takes
-- the source texts of all the benchmarks as parameters. See also Note
-- [Benchmark code structure]. Assumes the benchmark names in the list
-- are unique.
genBenchmarkCode :: Bool -> [(String, Program ())] -> IO (Program ())
genBenchmarkCode opaqueopt benches =
  let newSuite = new (var "Benchmark" `dot` "Suite") []
      benchesToObject bs = object $ map mkProp bs
      mkProp (s,j) = value (propS s) $ lambda [] $ unProgram j
      mvn = "monitor8asb"
      opt = if opaqueopt then Opaque else Transparent
  in do inlinedBenches <- runInliner2 $ mapM
                          (\(n, j) -> inlineEval mvn False j >>= return . (n,)) benches
        let [ExprStmt _ (CallExpr _ insBenGen [])] =
              unProgram $ redef $
              addRTS (def {stopfn = Exception, rtsOptimization = opt}) mvn $ program
              [returns $ benchesToObject inlinedBenches]
        return $
          program [vardecls [varinit "Benchmark" $ call (var "require") ["benchmark"]
                            ,varinit "uninstrumentedSuite" newSuite
                            ,varinit "instrumentedSuite" newSuite
                            ,varinit "uninstrumentedBenches" $ benchesToObject benches
                            ,varinit "instrumentedBenches" $ call insBenGen []
                            ]
                 ,forin (ForInVar $ vardecl "b") (var "uninstrumentedBenches") $ block
                  [expr $ call (var "uninstrumentedSuite" `dot` "add") [var "b", var "uninstrumentedBenches" `brack` var "b"]
                  ,expr $ call (var "instrumentedSuite" `dot` "add" ) [var "b", var "instrumentedBenches" `brack` var "b"]
                  ]
                 ,expr $ call (var "uninstrumentedSuite" `dot` "run") []
                 ,expr $ call (var "instrumentedSuite" `dot` "run") []
                 ,vardecls [varinit "result" $ object []]
                 ,for (VarInit [varinit "i" $ int 0]) (Just $ var "i" `lt` (var "uninstrumentedSuite" `dot` "length")) (Just $ postinc $ var "i") $ block
                  [vardecls [varinit "name" $ var "uninstrumentedSuite" `brack` (var "i") `dot` "name"]
                  ,ifte (var "name" `stneq` (var "instrumentedSuite" `brack` (var "i") `dot` "name"))
                   (throw "The names of instrumented and uninstrumented benchmarks don't match!'")
                   empty
                  ,expr $ (var "result" `brack` var "name") `assign`
                   object [value (propId "instrumented")
                           ((var "instrumentedSuite" `brack` var "i") `dot` "stats")
                          ,value (propId "uninstrumented")
                           ((var "uninstrumentedSuite" `brack` var "i") `dot` "stats")
                          ]
                  ]
                 ,expr $ call (var "console" `dot` "log") [call (var "JSON" `dot` "stringify") [var "result"]]
                 ]

runInliner2 :: Inliner a -> IO a
runInliner2 m = runInliner m >>= \case Left err -> fail $ show err
                                       Right a  -> return a
