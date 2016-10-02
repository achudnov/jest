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
module Inliner.CodeGen (valueof
                       ,levelof
                       ,lleq
                       ,varRead
                       ,remember
                       ,restore
                       ,assignVarWithOp
                       ,assignFieldWithOp
                       ,toBooleanV
                       ,varUnaryAssignOpM
                       ,fieldUnaryAssignOpM)  where

import Language.ECMAScript5.Syntax
import Language.ECMAScript5.Syntax.CodeGen
import Data.Default.Class
import Inliner.Monad
import RTS.Monitor.Core hiding (remember, restore)
import SyntaxHelpers
import Control.Applicative
import Control.Arrow
import Control.Monad
import RTS.Names
import Control.Monad.Reader
import RTS.Monitor.APISpec

valueof :: (Default a) => Expression a -> Expression a
valueof e = DotRef def e (Id def "v")

levelof :: (Default a) => Expression a -> Expression a
levelof e = DotRef def e (Id def "l")

fieldGet :: (Default a) => Expression a -> Expression a
fieldGet e = CallExpr def (DotRef def e (Id def "get")) []

fieldSet :: (Default a) => Expression a -> Expression a -> Expression a
fieldSet e v = CallExpr def (DotRef def e (Id def "set")) [v]

-- | For use whenever the ES semantics requries a ToBoolean dynamic
-- conversion
toBooleanV :: (Default a) => Expression a -> Rewriter (Expression a)
toBooleanV = liftM valueof . toBooleanBoxM

-- | generates an expression that compares it's arguments as levels
lleq :: (Default a) => Expression a -> Expression a -> Expression a
lleq e1 e2 = CallExpr def (DotRef def e1 (Id def "leq")) [e2]

-- | masks non-existence of a variable when reading it
varRead :: Default a => String -> Rewriter (Expression a)
varRead varName =
  do iv <- initVarM
     global <- globalProxyM
     return $ call
       (lambda []
        [trycatch
         [returns $ var $ ident varName]
         ("x", [returns $ assign (global `dot` "v" `dot` ident varName) iv]) Nothing]) []

globalProxyM :: (Default a, MonitorPrefix p, MonadReader p m) => m (Expression a)
globalProxyM = monitorComponentRefM globalProxyName

remember :: (Default a) => Int -> Rewriter (Statement a)
remember id = ExprStmt def <$> rememberM (int (fromInteger $ toInteger id))

restore :: (Default a) => Int -> Rewriter (Statement a)
restore id = ExprStmt def <$> restoreM (int (fromInteger $ toInteger id))

-- | A wrapper around the 'assignVar' and 'assignVarOp' binding that
-- converts the assignment op to a reference to the corresponding
-- binary op implementation in the monitor core.
assignVarWithOp :: (Default a) => AssignOp -> Expression a -> Expression a ->
                   Rewriter (Expression a)
assignVarWithOp op lhs rhs =
  case op of
    OpAssign -> assignVarM lhs rhs
    _ -> infixOpRefM (aop2iop op) >>= (\fop -> assignVarOpM fop lhs rhs)
  
-- | A wrapper around the 'assignField' and 'assignFielOp' binding that
-- converts the assignment op to a reference to the corresponding
-- binary op implementation in the monitor core.
assignFieldWithOp :: (Default a) => AssignOp -> Expression a -> Expression a ->
                     Expression a -> Rewriter (Expression a)
assignFieldWithOp op lhsobj lhsfield rhs =
  case op of
    OpAssign -> assignFieldM lhsobj lhsfield rhs
    _ -> infixOpRefM (aop2iop op) >>= (\fop -> assignFieldOpM fop lhsobj lhsfield rhs)

aop2iop aop = case aop of
  OpAssignAdd -> OpAdd
  OpAssignSub -> OpSub
  OpAssignMul -> OpMul
  OpAssignDiv -> OpDiv
  OpAssignMod -> OpMod
  OpAssignLShift -> OpLShift
  OpAssignSpRShift -> OpSpRShift
  OpAssignZfRShift -> OpZfRShift
  OpAssignBAnd -> OpBAnd
  OpAssignBXor -> OpBXor
  OpAssignBOr  -> OpBOr

varUnaryAssignOpM :: (Default a) => UnaryAssignOp -> Expression a -> Rewriter (Expression a)
varUnaryAssignOpM op = case op of
                        PrefixInc  -> varprefixincM
                        PrefixDec  -> varprefixdecM
                        PostfixInc -> varpostfixincM
                        PostfixDec -> varpostfixdecM
  

fieldUnaryAssignOpM :: (Default a) => UnaryAssignOp -> Expression a -> Expression a -> Rewriter (Expression a)
fieldUnaryAssignOpM op = case op of
                          PrefixInc  -> fieldprefixincM
                          PrefixDec  -> fieldprefixdecM
                          PostfixInc -> fieldpostfixincM
                          PostfixDec -> fieldpostfixdecM
