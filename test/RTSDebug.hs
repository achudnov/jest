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


{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Main where

import RTS
import RTS.Policy
import RTS.Monitor.Core
import Language.ECMAScript5.Syntax
import Language.ECMAScript5.Syntax.QuasiQuote
import Language.ECMAScript5.Syntax.CodeGen
import Language.ECMAScript5.PrettyPrint
import SyntaxHelpers
import System.Console.CmdArgs.Explicit
import Data.Default.Class
import Data.Default.Instances.Base

main :: IO ()
main = do params <- processArgs arguments
          let md = pmode params
          case md of
            RTSRepl -> printRTSRepl $ env params
            RTSTypes -> printRTSTypes $ env params
            MonitorTypes -> printMonitorTypes
            RTSDebugger -> printRTSDebugger $ env params

data Parameters = Parameters {pmode :: PMode
                             ,env :: Environment}

data PMode = RTSRepl | RTSDebugger | RTSTypes | MonitorTypes

arguments :: Mode Parameters
arguments = (modes "rtsdebug" defaultRTSRepl
   "RTS debugging tools for JEST"
   [(modeEmpty defaultRTSRepl) {modeNames = ["rtsrepl", "rr"]
                               ,modeHelp = "Prints a REPL for the RTS"
                               ,modeArgs = ([], Nothing)
                               ,modeGroupFlags = toGroup flags}
   ,(modeEmpty defaultRTSDebugger) {modeNames = ["rtsdebugger", "rd"]
                               ,modeHelp = "Prints the RTS in HTML to be loaded in a browser debugger"
                               ,modeArgs = ([], Nothing)
                               ,modeGroupFlags = toGroup flags}
   ,(modeEmpty defaultRTSTypes) {modeNames = ["rtstypes", "rt"]
                                ,modeHelp = "Prints the RTS annotated with types"
                                ,modeArgs = ([], Nothing)
                                ,modeGroupFlags = toGroup flags}
   ,(modeEmpty defaultMonitorTypes) {modeNames = ["monitortypes", "mt"]
                                    ,modeHelp = "Prints the Monitor Core annotated with types"
                                    ,modeArgs = ([], Nothing)
                                    ,modeGroupFlags = toGroup flags}    
   ])
  where defaultRTSRepl = Parameters RTSRepl Standalone
        defaultRTSDebugger = Parameters RTSDebugger Standalone
        defaultRTSTypes = Parameters RTSTypes Standalone
        defaultMonitorTypes = Parameters MonitorTypes Standalone
        flags = [flagBool ["b", "browser"] addBrowserEnvironment
                 "Assume browser execution environment. This flag is on by default for proxy mode and HTML content in batch mode"
                ]
        addBrowserEnvironment b params = 
          if b then params{env = Browser} else params

-- | Generates the monitor RTS according the the specified parameters,
-- with a 'read-eval-print' loop in it and pretty-prints it on stdout.
printRTSRepl :: Environment -> IO ()
printRTSRepl env =
  do let rp = def {stopfn = Exception, environment = env}
     let body =redef [js|print("JEST RTS REPL. Type 'quit()' to quit.");
                         while(true) {
                            putstr("> ");
                            var r = eval(readline());
                            if (typeof r !== 'undefined') print(r);
                         }
                        |]
     let prelude = addRTS rp "" body :: Program ()
     putStrLn (show $ prettyPrint prelude)

printRTSDebugger :: Environment -> IO ()
printRTSDebugger env =
  do let rp = def {stopfn = Exception, environment = env}
     let prelude = addRTS rp "" (Program () []) :: Program ()
     let output = "<html><body><script>" ++ (show $ prettyPrint prelude) ++
                  "</script></body></html>"
     putStrLn output
     

printRTSTypes :: Environment -> IO ()
printRTSTypes env =
  let rp = def {stopfn = Exception, environment = env}
  in putStrLn $ show $ prettyPrint $ addRTS rp "" (program [] :: Program ())

printMonitorTypes :: IO ()
printMonitorTypes =
  putStrLn $ show $ prettyPrint (monitorCore :: Program ())
