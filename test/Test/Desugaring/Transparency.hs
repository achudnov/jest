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


module Test.Desugaring.Transparency where

import Test.QuickCheck
import Language.ECMAScript5.Syntax
import Language.ECMAScript5.Syntax.Arbitrary
import Inliner.Desugaring
import Data.Generics.Uniplate.Data
import Data.Data (Data)
import Data.Default.Instances.Base
import Test.JSQuickCheck
import Test.Executable
import Control.Monad
import Test.QuickCheck.Monadic
import Test.Tasty
import Test.Tasty.QuickCheck

test_desugaring_transparency :: TestTree
test_desugaring_transparency =
  testProperty "Desugared programs are bisimilar to original" prop_exec_transparency

prop_exec_transparency :: Program () -> Property
prop_exec_transparency js =  undefined -- monadicIO $ assert $ 
--   where testCase = def {description = "Transparency test for desugaring"
--                        ,params = map ((::: arbValue) . unId) $ freeVars js
--                        ,body = mkTestBody js
--                        }
--         mkTestBody = foldl combine (Script () [])
--                      [pre, js, mkOutputs (vars js), between, desugar js,
--                       mkOutputs (vars $ desugar js), after]
--         mkOutputs = Script () .
--                     map (\id -> ExprStmt () $ CallExpr ()
--                                 (VarRef () $ Id () "output") [VarRef () id])
--         vars s = [id | VarRef _ id <- universeBi s] ++
--                  [Id a n | LVar a n <- universeBi s]

-- pre = removeAnnotations [js|var outputs = [[]];
--                          var round = 0;
--                          var output = function (x) {
--                            var o;
--                            if ((typeof x === "object") && x != null && "v" in x) 
--                              o = x.v; 
--                            else o = x;
--                            outputs[round].push(o);
--                          };
--                          var nextRound = function () {
--                            outputs[++round] = [];
--                          };
--                          var veriRounds = function (c) {
--                            for (var i = 1; i <= round; i++)
--                              c.assert(outputs[0].length === outputs[i].length);
--                            for (i = 0; i <= round; i ++)
--                              for (var j = 0; j <= outputs[0].length; j++)
--                                c.assert(outputs[0][j] === outputs[i][j]);
--                          };|]

-- between = removeAnnotations [js|nextRound();
--                                 monitorremember(1);
--                                 output = monitor.low(output);|]

-- after =  removeAnnotations [js|veriRounds(c);
--                                monitorrestore(1);|]
