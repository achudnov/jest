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


{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes, TupleSections, TemplateHaskell #-}

module Test.RTS.Core (test_core) where

import Test.Tasty
import Test.Common
import Test.RTS.Common
import Test.JSQuickCheck hiding (environment)
import Test.Executable
import Language.ECMAScript5.Syntax
import Language.ECMAScript5.Syntax.QuasiQuote
import Language.ECMAScript5.Syntax.Annotations
import Language.ECMAScript5.Syntax.CodeGen
import Data.Default.Class
import RTS
import RTS.Level
import RTS.Monitor.Core
import Test.Transparency.Generator
import Inliner.CodeGen
import Inliner.Monad
import Control.Monad.Reader
import RTS.Monitor.APISpec
import Control.Applicative

test_core :: TestTree
test_core = testGroup "Monitor Core executable tests" $
            map testWithBitVector test_cases

test_cases :: [JSProp]
test_cases = [JSProp "nsuCheck NSU restriction"
              ["b" ::: arbBox, "pl" ::: arbLevel] $
              udef  [js|monitorpush({l: pl},1);
                        var stopped = false;
                        try {
                           monitornsuCheck(b);
                        } catch (ex){
                          if (ex == "Security violation")
                            stopped = true;
                        }
                        c.assert((!pl.leq(b.l)) === stopped);
                        monitorpop(null,1);|]
             ,JSProp "nsuxCheck NSUX restriction"
              ["o" ::: arbObject, "f" ::: arbString, "pc" ::: arbLevel, "e" ::: arbBool]
              (udef [js|c.guard(f.length > 0);
                        var ob = monitorautolow(o);
                        var fb = monitorautolow(f);
                        function eq (l1, l2) {
                           return (l1.leq(l2) && l2.leq(l1));
                        }
                        monitorpush({l: pc},1);
                        var stopped = false;
                        if (e) ob.v[fb.v] = 1;
                        try {
                          monitornsuxCheck(ob, fb);
                        } catch (ex) {
                          if (ex == "Security violation")
                            stopped = true;
                        }
                        c.assert(!(eq(o.l, monitorlowlevel()) &&
                                   eq(f.l, monitorlowlevel()) &&
                                   eq(pc, monitorlowlevel())) && !e
                                 === stopped);
                        monitorpop(null,1);|])
             ,JSProp "autobox produces only correct values" ["v" ::: arbValue]
              (udef [js|var box = monitorautolow(v);
                        c.assert(isValidBox(box));
                        c.assert(implies(v instanceof Object, box.v instanceof monitorglobalProxy.Object.v));
                        c.assert(implies(v instanceof Array, box.v instanceof monitorglobalProxy.Array.v));
                        function implies (x, y) {
                           return !(x && !y);
                        }
                       |])
             ,JSProp "topctx always points to the last element of the pcls array, if such element exists"
             ["npushes" ::: arbWholeNum, "npops" ::: arbWholeNum, "nremrest" ::: arbWholeNum]
             (udef [js|function psh() {
                         if (npushes > 0) {
                            npushes--;
                            var l = arbLevel.arb(31); 
                            monitorpush({l:l}, npushes);
                         }
                       }
                       function pp() {
                         if (npops > 0) {
                            npops--;
                            if (topctx)  monitorpop(null, topctx.id);
                         }
                       }
                       var nremembers = nremrest;
                       var nrestores = nremrest;
                       function rem() {
                         if (nremembers > 0) {
                            nremembers--;
                            monitorremember(nremembers);
                         }
                       }
                       function rest() {
                         if (nrestores > 0) {
                            nrestores--;
                            monitorrestore(nrestores);
                         }
                       }
                       while (npushes > 0 || npops > 0 || nremembers > 0 || nrestores > 0) {
                          c.assert(monitortopctx == (monitorpcls.length > 0 ? monitorpcls[monitorpcls.length - 1] : null));
                          var action = choose(psh, pp, rem, rest);
                          action();
                       }
                      |])
             ,JSProp "if there are any contexts on pcls then pclabel is always the label of the top context, bottom label otherwise"
             ["npushes" ::: arbWholeNum, "npops" ::: arbWholeNum, "nremrest" ::: arbWholeNum]
              (udef [js|function psh() {
                         if (npushes > 0) {
                            npushes--;
                            var l = arbLevel.arb(31); 
                            monitorpush({l:l}, npushes);
                         }
                       }

                       function pp() {
                         if (npops > 0) {
                            npops--;
                            if (topctx) monitorpop(null, topctx.id);
                         }
                       }
                       var nremembers = nremrest;
                       var nrestores = nremrest;
                       function rem() {
                         if (nremembers > 0) {
                            nremembers--;
                            monitorremember(nremembers);
                         }
                       }
                       function rest() {
                         if (nrestores > 0) {
                            nrestores--;
                            monitorrestore(nrestores);
                         }
                       }
                       while (npushes > 0 || npops > 0 || nremembers > 0 || nrestores > 0) {
                          c.assert(monitorpclabel == (monitortopctx ? monitortopctx.label : monitorlowlevel()));

                          var action = choose(psh, pp, rem, rest);
                          action();
                       }
                       |])
             ,JSProp "The labels of contexts increase monotonically"
             ["npushes" ::: arbWholeNum, "npops" ::: arbWholeNum]
              (udef [js|function psh() {
                         if (npushes > 0) {
                            npushes--;
                            var l = arbLevel.arb(31);
                            var ipd = arbWholeNum.arb(100);
                            monitorpush({l:plevel},ipd);
                         }
                       }

                       function pp() {
                         if (npops > 0) {
                            npops--;
                            if (topctx) monitorpop(null, topctx.id);
                         }
                       }

                       function invariant () {
                         for (var i = 0; i < monitorpcls.length; i++)
                            for (var j = i; j < monitorpcls.length; j++)
                                c.assert(monitorpcls[i].label.leq(monitorpcls[j].label));
                       }

                       while (npushes > 0 || npops > 0) {
                          invariant();
                          var action = choose(psh, pp);
                          action();
                       }
                 |])
             ,JSProp "PCLS' label is at least the pushed label"
              ["l" ::: arbLevel, "i" ::: arbWholeNum]
              (udef [js|monitorpush({l: l}, i);
                        c.assert(l.leq(monitorpclabel));
                        monitorpop(null, i);|])
             ,JSProp "pop reverses PCLS' level"
              ["l1" ::: arbLevel, "l2" ::: arbLevel]
              (udef [js|monitorpush({l: l1}, 1);
                        var l = monitorpclabel;
                        monitorpush({l: l2}, 2);
                        monitorpop(null, 2); 
                        c.assert(eq(monitorpclabel, l));
                        monitorpop(null, 1);|])
             ,JSProp "Push to the same ID doesn't create a new label"
              ["l1" ::: arbLevel, "l2" ::: arbLevel, "l3" ::: arbLevel]
              (udef [js|monitorpush({l: l1}, 1);
                        var l = monitorpclabel;
                        monitorpush({l: l2}, 2);
                        monitorpush({l: l3}, 2);
                        c.assert(eq(monitorpclabel, l1.join(l2).join(l3)));
                        monitorpop(null, 2);
                        c.assert(eq(monitorpclabel, l));
                        monitorpop(null, 1);
                       |])
             ,JSProp "Remember remembers and restore restores"
              (map (::: arbLevel) ["l1", "l2", "l3", "l4"] ++
               map (::: arbInt) ["id1", "id2"])
              (udef [js|monitorpush({l: l1}, 1);
                        var lev1 = monitorpclabel;
                        monitorremember(id1);
                        monitorpush({l: l2}, 2);
                        monitorpush({l: l3}, 3);
                        var lev2 = monitorpclabel;
                        monitorremember(id2);
                        monitorpush({l: l4}, 4);
                        monitorrestore(id2);
                        c.assert(eq(monitorpclabel, lev2));
                        monitorrestore(id1);
                        c.assert(eq(monitorpclabel, lev1));
                        monitorpop(null, 1);//reset pcls
                       |])
             ] 
             ++ (map genInfixTest $ filter (`notElem` excludedInfixOps) $ map snd $ $(enumOps ''InfixOp))
             ++ (map genPrefixTest $ filter (`notElem` excludedPrefixOps) $ map snd $ $(enumOps ''PrefixOp))
             ++ (map genUnaryAssignTest $ filter (`notElem` excludedUnaryAssignOps) $ map snd $ $(enumOps ''UnaryAssignOp))


-- | What are we testing:
-- * the value of the resulting box is obtained by applying the
--   operator to the initial values of boxes
-- * level of the resulting box should be the join of levels of
--   argument boxes
-- * the resulting box is valid

genInfixTest :: InfixOp -> JSProp
genInfixTest op =
  let vgold = "vgold"
      lgold = "lgold"
      x     = var "x"
      y     = var "y"
      result= "result"
      assert e = ExprStmt () $ CallExpr () ((var "c") `dot` "assert") [e]
  in  JSProp ("Infix operator integrated correctness: " ++ show op) ["x" ::: arbBox, "y" ::: arbBox]$
      [VarDeclStmt ()
       [VarDecl () vgold $ Just $ InfixExpr () op (valueof x) (valueof y)
       ,VarDecl () lgold $ Just $ CallExpr () ((levelof x) `dot` "join") [levelof y]
       ,VarDecl () result $ Just $ infixOpM op x y ("monitor" :: String)
       ]
      ,assert $ InfixExpr () OpStrictEq (valueof $ VarRef def result) (VarRef () vgold)
      ,assert $ call (var "eq") [levelof $ VarRef def result, var lgold]
      ,assert $ CallExpr () (var "isValidBox") [VarRef def result]
      ]

genPrefixTest :: PrefixOp -> JSProp
genPrefixTest op =
  let vgold = "vgold"
      x     = var "x"
      result= "result"
  in  JSProp ("Prefix operator integrated correctness: " ++ show op) ["x" ::: arbBox]$
      [VarDeclStmt ()
       [VarDecl () vgold $ Just $ PrefixExpr () op (valueof x)
       ,VarDecl () result $ Just $ prefixOpM op x ("monitor"::String)
       ]
      ,ExprStmt () $ assert $ InfixExpr () OpStrictEq (valueof $ VarRef def result) (VarRef () vgold)
      ,ExprStmt () $ assert $ call (var "eq") [levelof $ VarRef def result, levelof x]
      ,ExprStmt () $ assert $ CallExpr () (var "isValidBox") [VarRef def result]
      ]

varUnaryAssignOp :: (Default a) => UnaryAssignOp -> Expression a -> String -> Expression a
varUnaryAssignOp op = case op of
  PrefixInc  -> varprefixincM
  PrefixDec  -> varprefixdecM
  PostfixInc -> varpostfixincM
  PostfixDec -> varpostfixdecM


genUnaryAssignTest :: UnaryAssignOp -> JSProp
genUnaryAssignTest op =
  let vgold = "vgold"
      x     = var "x"
      result= "result"
      l     = var "l"
  in  JSProp ("Unary assignment operator integrated correctness: " ++ show op) ["x" ::: arbBox, "l" ::: arbLevel]$
      [VarDeclStmt ()
       [VarDecl () vgold $ Just $ UnaryAssignExpr () op $ valueof x
       ,VarDecl () result $ Just $ varUnaryAssignOp op x "monitor"
       ]
      ,ExprStmt () $ pushFunc $ ObjectLit ()
       [PValue () (PropId () "l") l]
      ,ExprStmt () $ assert $ InfixExpr () OpStrictEq (valueof $ VarRef def result) (VarRef () vgold)
      ,ExprStmt () $ assert $ call (var "eq") [levelof $ VarRef def result, CallExpr () (DotRef () (levelof x) "join") [l]]
      ,ExprStmt () $ assert $ CallExpr () (var "isValidBox") [VarRef def result]
      ]

-- | We exclude them from testing because there is not implementation
-- of them in the core -- it's done by rewriting
excludedInfixOps = [OpLAnd, OpLOr]

excludedPrefixOps = [PrefixDelete, PrefixVoid]

excludedUnaryAssignOps = []
