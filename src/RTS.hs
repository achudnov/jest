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


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable, QuasiQuotes, TypeFamilies #-}

-- | Generates the monitor Run-Time System.
module RTS (addRTS
           ,addRTSAdvanced
           ,module RTS.Parameters
           ,module Policy
           ) where

import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map)
import MyPrelude
import Language.ECMAScript5.Syntax
import Language.ECMAScript5.Syntax.Annotations
import Language.ECMAScript5.Syntax.CodeGen
import Language.ECMAScript5.Syntax.QuasiQuote
import Data.Data (Data)
import Data.Typeable (Typeable)
import qualified RTS.APIProxy.ESSL as ESSL
import qualified RTS.APIProxy.DOM  as DOM
import qualified RTS.APIProxy.Android  as Android
import RTS.Monitor.Core
import Data.Default.Class
import Control.Arrow ((>>>))
import RTS.Policy (Policy)
import RTS.Level (LevelImplementation)
import qualified RTS.Policy as Policy (compile)
import Network.URI (URI)
import Data.Monoid
import RTS.Parameters
import SyntaxHelpers
import SyntaxHelpers.JSLink
import Data.Maybe (catMaybes)
import Inliner.CodeGen
import Control.Monad.Except
import qualified SyntaxHelpers.JSFunc as JSFunc
import SyntaxHelpers.JSFunc (JSFunc)
import RTS.Names

{- Note [Structure of a transformed program]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
At the top level, any transformed JavaScript program looks as follows:

(function () {
   var <prefix>policy = <...>;
   var <prefix>Level  = <...>;
   var <prefix>global = new GlobalProxy();
   <Monitor declarations>;
   function <prefix>stop (file, line, col, reason) {..}
   return (function () {
     var <API lockout vars>;
     with (this.v) {<program>};
   }).call(<prefix>global);
})();
-}

-- | Adds the monitor RTS to the program. Establishes the executable
-- environment the instrumented program expects: the monitor core is
-- initialized and accessible via the specified variable name, all the
-- API's are information-flow aware and operate on boxes. See also
-- Note [Structure of a transformed program]
addRTS :: (Default a, Data a, Typeable a, Monoid a) => Params -> String -> Program a -> Program a
addRTS pp prefix js =
  program [expr $ JSFunc.call (addRTSAdvanced pp prefix [] [] id js) []]

-- | A more flexible RTS constructor, for use in the test case generator
addRTSAdvanced :: (Default a, Data a, Typeable a, Monoid a)
               => Params -- ^ RTS parameters
               -> String -- ^ Monitor prefix
               -> [Statement a] -- ^ Extra code to include after the
                                -- RTS initialization
               -> [String] -- ^ Parameter names for the outer function
               -> (Expression a -> Expression a)
               -- ^ Transformer for the return expression of the outer
               -- function
               -> Program a -- ^ Program to be wrapped
               -> JSFunc a
addRTSAdvanced pp prefix extras params returnTransform (Program _ prg)=
  let (proxycon, tops) = environmentSpec $ environment pp
      renamer = (prefix ++)
      rts = unProgram $ renameDecls renamer $ link renamer
            [program [vardecls [varinit "policy" $ Policy.compile (policy pp)
                               ,varinit "Level"  $ var "policy" `dot` "levelImpl"
                               ,varinit globalProxyName $ new (JSFunc.compile proxycon) []
                               ]]
            ,patchMonitorPrefixVar prefix $
             (if environment pp == Standalone then disableInlineEvalXHR else id)
             $ patchStopFunction (stopfn pp) monitorCore
            ]
      apiEnvironmentInitializer = case rtsOptimization pp of
        Transparent -> [vardecls $ map (vardecl . ident) tops
                       ,with (valueof this) $ block prg]
        Opaque      ->
          (vardecls $ map (\n -> varinit (ident n) $ globalProxyRef `dot` "v" `dot` (ident n)) tops):prg
      globalProxyRef = monitorComponentRefM globalProxyName prefix
  in JSFunc.JSFunc params $ rts ++ extras ++
     [returns $ returnTransform $ call
      ((lambda [] apiEnvironmentInitializer) `dot` "call")
      [globalProxyRef]
     ]

-- | Fills in the empty body of the existing stop function in the
-- Monitor Core
patchStopFunction :: (Default a) => StopFunction -> Program a -> Program a
patchStopFunction sf (Program a ss) = Program a $ map patch ss
  where patch (FunctionStmt a id@(Id _ "stop") params []) =
          FunctionStmt a id params $ stopFunction sf
        patch s = s

patchMonitorPrefixVar :: (Default a) => String -> Program a -> Program a
patchMonitorPrefixVar pre (Program a ss) = Program a $ map patch ss
  where patch (VarDeclStmt a [VarDecl b id@(Id _ "monitorPrefix") _]) =
          VarDeclStmt a [VarDecl b id $ Just $ string pre]
        patch s = s

-- | The current implementation of inlining in eval requires a call to
-- the proxy server, which is implemented with XMLHttpRequest,
-- normally available in browsers only, so we need to disable it in a
-- standalone (ESSL) mode. This is what this function does.
disableInlineEvalXHR :: Default a => Program a -> Program a
disableInlineEvalXHR (Program a ss) = Program a $ map patch ss
  where patch (VarDeclStmt a [VarDecl b id@(Id _ "inlineEvalXHR") _]) =
          VarDeclStmt a [VarDecl b id $ Just $ NullLit def]
        patch s = s

stopFunction :: (Default a) => StopFunction -> [Statement a]
stopFunction sf = unProgram $ redef $
  case sf of
   Loop      -> [js|while (true) {};|]
   Report    -> [js|alert("NSU Violation at " + file + ":" + line + ","
                    + col + " because " + reason);|]
   Exception -> [js|throw "NSU Violation at " + file + ":" + line + ","
                    + col + " because " + reason;|]

-- | produces an API spec (global object spec) for the given execution
-- environment
environmentSpec :: Environment -> (JSFunc (), [String])
environmentSpec env = case env of
  Standalone    -> ESSL.proxyInit
  Browser       -> DOM.proxyInit
  Android       -> Android.proxyInit
  Custom base js -> let (JSFunc.JSFunc params body, vars) = environmentSpec base
                    in (JSFunc.JSFunc params $ (init body) ++ unProgram js ++ [last body], vars)
                       
    
