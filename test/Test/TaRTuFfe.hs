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
-- | Two-run test framework. For testing both non-interference (or
-- richer notions of security) and transparency. The idea is to have
-- two "runs", but not necessarily of the same program: e.g. for NI we
-- want two runs of the same program, with some inputs ("high" inputs)
-- varied between the two runs, but for transparency we need to run an
-- "original" program and then some transformation of the original
-- program with the same inputs. But in both versions the outputs
-- should match. We use JavaScript QuickCheck (module
-- Test.JSQuickCheck) for "varying" the inputs.

module Test.TaRTuFfe where

import Test.Executable
import Test.JSQuickCheck
import Test.Common
import RTS hiding (environment)
import Language.ECMAScript5.Syntax
import Language.ECMAScript5.Syntax.Annotations
import Language.ECMAScript5.Syntax.QuasiQuote
import Data.Default.Class
import SyntaxHelpers.JSFunc

data TRCase = TRCase {caseName :: String
                     ,caseParams :: [(Parameter, Bool)]
                      -- ^ The boolean is True if the parameter if
                      -- HIGH, i.e. it should change between runs
                     ,firstRun :: JSFunc ()
                      -- ^ Should be a function. See note
                      -- [Expectations for programs under test]
                     ,secondRun :: JSFunc ()
                      -- ^ Should be a function. See note
                      -- [Expectations for programs under test]
                     }

{- Note [Expectations for programs under test]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
- pure function
- no exceptions thrown
- takes only declared parameters
-}

{- Note [Parameters]
~~~~~~~~~~~~~~~~~~~~
For NI testing we need to vary some inputs between runs, and fix the
others. We would also like to not write that code from scratch and
reuse the existing JSQuickCheck. The solution is that we formulate a
2-run tests as one QuickCheck property with parameters constructed as
follows: take all the list "low" params and twice the list of
"high". Provide the low params and the first half of "high" params to
the first run, and the low params and the second half of the "high"
params to the second run. That way we have both fixed and variable
params for both runs, and all of them are random.
-}

compileTRCase :: TRCase -> TestCase
compileTRCase trc = def {description = caseName trc
                        ,parameters = mkQCParams $ caseParams trc
                        ,body = compileBody (firstRun trc) (secondRun trc)
                        }
  where compileBody :: JSFunc () -> JSFunc () -> [Statement ()]
        compileBody fr sr =
          [mkInvoke "run1" fr $ mkRunParams (caseParams trc) 1
          ,mkInvoke "run2" sr $ mkRunParams (caseParams trc) 2
          ,ExprStmt def $ assert $ CallExpr def comparisonFn $ map (VarRef def . Id def) ["run1", "run2"]
          ]
        -- The comparison function for outputs (deep comparison of
        -- values)
        comparisonFn = redef [jsexpr|function deepCmp(x, y){
                           var sameProperties = function (o1, o2) {
                              var result = true;
                              for (var f in o1) result = result && f in o2;
                              for (var f in o2) result = result && f in o1;
                              if (result) for (f in o1) result = result && deepCmp(o1[f], o2[f]);
                              return result;
                           };
                           if (typeof(x) != "object" && typeof(y) != "object")
                             return x === y;
                           else return (typeof(x) === typeof(y)) &&
                                        sameProperties(o1, o2);
                       }|]
        mkInvoke var fn params =
          VarDeclStmt def [VarDecl def (Id def var) $ Just $ call fn params]
        -- Make a parameter list for a given run number. See Note [Parameters]
        mkRunParams params runN =
          map (\((name ::: _), high) -> VarRef def $ Id def $
                                        if high then name ++ show runN
                                        else name) params
        -- Expands 2RTF params into QuickCheck params. See Note [Parameters].
        mkQCParams = concatMap $ \(p@(name ::: arb), high) ->
          if high then [(name ++ "1") ::: arb, (name ++ "2") ::: arb] else [p]
