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


{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}

-- | Executable JavaScript tests
module Test.Executable (runJS, Outcome(..), JSExecTest(..), mkJSTest) where

import Test.Tasty
import Test.Tasty.Options
import Test.Tasty.Providers
import System.IO
import System.IO.Temp
import System.Process
import System.Exit (ExitCode(..))
import Language.ECMAScript5.Syntax
import Language.ECMAScript5.Syntax.Annotations
import Language.ECMAScript5.PrettyPrint
import Control.Applicative
import Data.Proxy
import Test.Tong
import Test.Common
import Data.Default.Class
import Data.Default.Instances.Base
import Data.Monoid
import Data.Typeable (Typeable)
import Options.Applicative hiding (Success)
import System.EasyFile
import System.Random
import Control.Monad
import Data.Char

data Outcome = Success String
             | Error String

-- | Runs a JavaScript program via an external interpreter (hardcoded
-- to 'js' currently). Returns either the 'stdout' (if success) or
-- 'stderr' (if failure)
runJS :: FilePath -> Program a -> IO Outcome
runJS jsiPath js = withSystemTempFile "test.js" runTest
  where runTest path handle =
          do hPutStr handle src
             hFlush handle
             (code, out, err) <- readProcessWithExitCode jsiPath ["-f", path] ""
             return $ if (code == ExitSuccess) then Success out
                                               else Error err
        src = show $ prettyPrint js

data JSExecTest = JSExecTest (Program ()) (String -> Bool)
                deriving (Typeable)

instance IsTest JSExecTest where
  run opts (JSExecTest js check) _ =
    let JSExecUseTong useTong = lookupOption opts
        JSExecInterpreterPath interpPath = lookupOption opts
        JSExecDumpSourceOnFailure dumpSource = lookupOption opts
        JSExecSavedSourcePrefix dumpPrefix = lookupOption opts
    in runJS interpPath js >>= \res -> case res of
      Success out -> return $
                     if check out then testPassed "" else testFailed out
      Error err   ->
        let runTrace = (runJS interpPath $ etrace js) >>=
                       \res2 -> return $ case res2 of
                         Success tr -> " Stack trace: " ++ tr
                         Error _    -> " Stack trace failed."
            sourceDumper = do file <- generateFreshFileName dumpPrefix
                              writeFile file $ show $ prettyPrint js
                              return file
        in do traceMessage <- if useTong then runTrace else return ""
              dumpMessage  <- if dumpSource
                              then liftM (" Source saved in " ++) sourceDumper
                              else return ""
              let message = traceMessage ++ dumpMessage
              return $ testFailed $ "Interpreter error." ++ message
  testOptions = return [Option (Proxy :: Proxy JSExecUseTong)
                       ,Option (Proxy :: Proxy JSExecInterpreterPath)
                       ,Option (Proxy :: Proxy JSExecDumpSourceOnFailure)
                       ,Option (Proxy :: Proxy JSExecSavedSourcePrefix)
                       ]

-- | Represents the option for enabling/disabling Tong tracing upon
-- test failures
newtype JSExecUseTong = JSExecUseTong Bool
                      deriving (Eq, Typeable)
                               
-- | Represents the option for providing a custom interpreter path
newtype JSExecInterpreterPath = JSExecInterpreterPath FilePath
                              deriving (Eq, Typeable)

-- | Represents the option of saving the source of the test-case to a
-- new file on failure.
newtype JSExecDumpSourceOnFailure = JSExecDumpSourceOnFailure Bool
                                  deriving (Eq, Typeable)


-- | Gives the ability to specify a custom directory prefix for saving
-- test-case sources JSExecDumpSourceOnFailure
newtype JSExecSavedSourcePrefix = JSExecSavedSourcePrefix FilePath
                                deriving (Eq, Typeable)

instance IsOption JSExecUseTong where
  defaultValue = JSExecUseTong False
  optionName = return "jse-use-tong"
  optionHelp = return "Whether to use exception tracing (Tong) on test failures."
  parseValue = fmap JSExecUseTong . safeRead
  optionCLParser = JSExecUseTong <$> switch (long "jse-use-tong" <> help "Whether to use exception tracing (Tong) on test failures.")

instance IsOption JSExecDumpSourceOnFailure where
  defaultValue = JSExecDumpSourceOnFailure False
  optionName = return "jse-dump-source"
  optionHelp = return "Whether to dump source into a new file on test failure"
  parseValue = fmap JSExecDumpSourceOnFailure . safeRead
  optionCLParser = JSExecDumpSourceOnFailure <$> switch (long "jse-dump-source" <> help "Whether to dump source into a new file on test failure")


instance IsOption JSExecInterpreterPath where
  defaultValue = JSExecInterpreterPath "js"
  optionName = return "jse-interpreter"
  optionHelp = return "Path to the JavaScript interpreter (only SpiderMonkey and Rhino are supported at this time"
  parseValue = fmap JSExecInterpreterPath . safeRead

instance IsOption JSExecSavedSourcePrefix where
  defaultValue = JSExecSavedSourcePrefix "."
  optionName = return "jse-dump-prefix"
  optionHelp = return "Prefix for test source dumps"
  parseValue = fmap JSExecSavedSourcePrefix . safeRead
  optionCLParser = JSExecSavedSourcePrefix <$> strOption (long "jse-dump-prefix" <> help "Prefix for test source dumps")



mkJSTest :: String -> Program a -> (String -> Bool) -> TestTree
mkJSTest name js check = singleTest name $ JSExecTest (removeAnnotations js) check

