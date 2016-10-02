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
{-# LANGUAGE ScopedTypeVariables, QuasiQuotes, TupleSections,
TemplateHaskell #-}
-- | Common infrastructure for transparency tests
module Test.Transparency.Common where

import Language.ECMAScript5.Syntax
import Language.ECMAScript5.Syntax.QuasiQuote
import Language.ECMAScript5.Syntax.Annotations
import Language.ECMAScript5.Syntax.CodeGen
import Data.Default.Class
import Data.Generics.Uniplate.Data
import Data.Data (Data)
import Data.Data (Typeable)
import Data.Set (Set)
import qualified Data.Set as Set
import Test.TaRTuFfe
import Control.Arrow
import Test.Common
import Test.JSQuickCheck
import SyntaxHelpers.JSFunc hiding (call)
import SyntaxHelpers
import RTS hiding (environment)
import Inliner
import Control.Monad

{- Note [Structure of transparency test cases]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In transparency two-run test cases the first run is always the
original code, the second is transformed code, wrapped in RTS with the
outer closure layer removed.

Adaptation of arguments is necessary for the second run -- valid boxes
should be produced from unboxed arguments.

Second run looks like this:

function (x, y, ...) {
   // RTS
   //...
   // parameter adaptation.
   x = adaptParam(monitor, x);
   y = adaptParam(monitor, y);
   ...
   return ToPrimitiveBox((function () {
     var <API lockout vars>;
     with (monitor.globalProxy) {<transformed test case>};
   }).call(global)).v; // adaptedReturn
}

-}

-- | Creates a Tartuffe test case where the first and the second run
-- are the same, modulo that the second is transformed with the
-- inliner. See Note [Structure of transparency test cases]. Warning:
-- relies heavily on the structure of transformed program (see Note
-- [Structure of a transformed program] in RTS) as well as the monitor
-- core API.
mkTransCase :: String -> [Parameter] -> Program a -> IO TRCase
mkTransCase name pars run =
  let mkJSFunc js = JSFunc (map (\(n ::: _) -> n) pars) $ unProgram $ redef js
      run1 = mkJSFunc run
  in (liftM (addRTS def "monitor") $ (runInliner $ inlineEval "monitor" False $ removeAnnotations run) >>= leftToFail) >>= \(Program _ [ExprStmt _ (CallExpr _ (FuncExpr _ _ _ body) [])]) ->
  let adaptedReturn = let ReturnStmt _ (Just innercall) = last body
                      in  returns $ (call (var "ToPrimitiveBox") [innercall]) `dot` "v"
      run2 = mkJSFunc $ program $ init body ++ liftParams "monitor" (map parameter_name pars) ++ [adaptedReturn]
  in return $ TRCase name (map (returnA &&& (arr $ const False)) pars) run1 run2
