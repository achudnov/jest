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


{-# LANGUAGE QuasiQuotes #-}
-- | Exports the parsed source code for Tracer Tong, a simple
-- JavaScript exception stack trace generator.

module Test.Tong (tong, etrace) where

import Language.ECMAScript5.Syntax.QuasiQuote
import Language.ECMAScript5.Syntax
import Language.ECMAScript5.PrettyPrint
import SyntaxHelpers
import Data.Default.Class

tong :: Default a => Program a
tong = redef [js|function etrace(code) {
           var glob = newGlobal("x");
           var dbg = new Debugger();
           var trace = [];
  dbg.onExceptionUnwind = function (frame, ex) {
    if (frame) 
      trace.push((frame.callee?frame.callee.name:"[top-level code]") + ":" + frame.script.getOffsetLine(frame.offset));
  };
  dbg.addDebuggee(glob);
  try {
    glob.evaluate(code, {fileName: "test", lineNumber: 1});
  } catch (ex) {
    print("Caught an exception: " + ex + "\n" + "Stack trace:");
    for (var i = 0; i < trace.length; i++) {
      print(trace[i]);
    }
  }
}|]

-- | Wraps a given JavaScript program in the tracer
etrace :: Default a => Program a -> Program a
etrace js = Program def $ unProgram tong ++ [ExprStmt def $ CallExpr def (VarRef def $ Id def "etrace") [StringLit def $ show $ prettyPrint js]]
