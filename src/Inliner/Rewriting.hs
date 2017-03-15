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


{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Inliner.Rewriting (rewrite) where

import           SyntaxHelpers.ASTTransformations
import           Control.Applicative
import           Control.Arrow                           (second)
import           Control.Monad hiding (sequence, mapM, guard)
import Prelude hiding (sequence, mapM)
import           Control.Monad.Trans                     (lift)
import           Data.Data                               (Data)
import           Data.Default.Class
import           Data.Default.Instances.Base
import           Data.Generics.Uniplate.Data             hiding (Transformer,
                                                          rewrite)
import           Data.Set                                (Set)
import qualified Data.Set                                as Set
import           Data.Traversable                        (Traversable, sequence, mapM)
import           Inliner.Annotations
import           Inliner.CodeGen
import           Inliner.Monad
import           Language.ECMAScript5.Analysis.LabelSet
import           Language.ECMAScript5.Syntax
import           Language.ECMAScript5.Syntax.Annotations
import           Language.ECMAScript5.Syntax.CodeGen
import           RTS.Monitor.Core hiding (remember, restore, exit, update, push)
import           SyntaxHelpers
import Lens.Simple hiding (assign)
import Data.Maybe (catMaybes)

-- | Transforms the program to inline the monitor
class Rewritable a where
  rewrite :: (Default b) => a (b, Annotation) -> Rewriter (a (b, Annotation))

instance Rewritable Program where
  rewrite (Program a body) = liftM (Program a) $ mapM rewrite body

instance Rewritable Expression where
  rewrite =
    let logicalOpCommon e1 e2 c =
          do e1' <- rewrite e1
             e2' <- rewrite e2
             let x = var "x"
             let lx = levelof x
             bx <- toBooleanV x
             res <- join2M e2' lx
             let f = cond bx
             let ce = if c then f res x else f x res
             return $ call (lambda ["x"] [returns ce]) [e1']
        -- | Generates an expression that returns true only if the
        -- deletion of the argument is allowed to be performed at
        -- run-time. The level of the PCLS should be low
        allowVarDeletion e = liftM2 lleq lowlevelM pclabelM
        -- | The level of object/array reference and the level of PCLS
        -- should both be low
        propertyDeletionRestriction obj =
          do ll <- lowlevelM
             let lo = levelof obj
             lp <- pclabelM
             return $ (lleq lo ll) `land` (lleq lp ll)
        exprCases e = case e of
            -- Transforms expressions so that the following invariant
            -- is maintained: every expression is in a boxed and
            -- levels of compound expressions are computed.
            StringLit _ _ -> primlowM e
            RegexpLit {}  -> primlowM e
            NumLit _ _ -> primlowM e
            BoolLit _ _ -> primlowM e
            NullLit _ -> primlowM e
            ArrayLit a elems -> liftM (ArrayLit a) (mapM (sequence . fmap rewrite) elems) >>= arraylowM
            ObjectLit a props -> liftM (ObjectLit a) (mapM rewrite props) >>= objectlowM
            FuncExpr a mid params body -> liftM (FuncExpr a mid params) (mapM rewrite body)
              >>= funlowM
            -- 'this' should already point to a boxed object
            ThisRef _ -> return e
            -- We box var references to an enclosing named function
            -- expression with low level because their value (the
            -- function) cannot be wrapped at the time of creation
            -- (and the variable cannot be written to). It is a low
            -- level, and not something else because the only way the
            -- var ref could have been reached is if the function it
            -- refers to was declared in the first place. So, there's
            -- not information flow channel from the PC to the
            -- self-reference in a named lambda.
            VarRef (_, ann) _ | ann^.transformAnn.lambdaSelfRef -> funlowM e
            VarRef _ _ -> return e
            -- variable and field values should already be in packed
            -- form packExpression e@(VarRef _ _) -> e join the level
            -- of the field value with the level of the object ref
            DotRef _ o f -> do f' <- primlowM $ StringLit def $ unId f
                               rewrite o >>= flip readFieldM f'
            -- join the level of the field value with the level of the
            -- object ref and the value of the key expression
            BracketRef _ o k -> do o' <- rewrite o
                                   k' <- rewrite k
                                   readFieldM o' k'
            CondExpr a g t el -> liftM3 (CondExpr a)
                                 (rewrite g >>= toBooleanV)
                                 (rewrite t) (rewrite el)
            -- Direct call to eval. We can only detect one of the two
            -- conditions for a direct call to eval: namely, that the
            -- reference to function has an environment record as it's
            -- base and the name of the reference is "eval" (spec
            -- 15.1.2.1.1). In other words, if the call is via a
            -- variable reference: "eval(...)". The other part ---the
            -- reference evaluating to the built-in 'eval' method---
            -- requires points-to analysis we don't have. So, if we
            -- detect the first condition, we rewrite the call a
            -- special way to check the second condition at run-time
            -- and perform an emulation of the direct call to eval.
            CallExpr _ (VarRef _ (Id _ "eval")) args ->
              do oe    <- originalEvalM
                 ifm   <- invokeFunctionM (var "eval") (var "arguments")
                 iepm  <- isEvalProxyM $ var "eval"
                 inl   <- inlineEvalM $ brack (var "arguments") (int 0)
                 liftM (call (lambda []
                              [ifte iepm
                               (returns $ call (lambda []
                                                [vardecls [varinit "eval" oe]
                                                ,expr $ call (var "eval") [inl]
                                                ]
                                               ) [brack (var "arguments") (int 0)])
                               (returns ifm)
                              ])) $ mapM rewrite args
            CallExpr _ (DotRef _ obj field) args ->
              do field' <- primlowM (id2string field)
                 obj' <- rewrite obj
                 args' <- mapM rewrite args
                 invokeMethodM obj' field' $ array $ map Just args'
            CallExpr _ (BracketRef _ obj field) args ->
              do obj' <- rewrite obj
                 field' <- rewrite field
                 args' <- mapM rewrite args
                 invokeMethodM obj' field' $ array $ map Just args'
            CallExpr a fun args ->
              -- We clear native error annotations for function calls
              -- because the implementation of the function call in
              -- the monitor already pushes the level of the function
              -- reference on the PC level stack, and keeping the
              -- annotation would force a redundant PC push.
              do fun' <- rewrite fun -- $ clearNativeErrors fun
                 args' <- mapM rewrite args
                 invokeFunctionM fun' $ array $ map Just args'
            NewExpr _ ctor args ->
              -- We clear native error annotations for new calls
              -- because the implementation of the new call in the
              -- monitor already pushes the level of the constructor
              -- reference on the PC level stack, and keeping the
              -- annotation would force a redundant PC push.
              do ctor' <- rewrite ctor -- $ clearNativeErrors ctor
                 args' <- mapM rewrite args
                 newObjectM ctor' $ array $ map Just args'
            -- Applies structural NSU restrictions to delete. Apply
            -- the operation, keep the level
            PrefixExpr a PrefixDelete v@(VarRef {}) -> rewrite v >>= \v' ->
              liftM3 cond (allowVarDeletion v') (return $ delete v')
              (stopM null_ null_ null_ null_)
            PrefixExpr a PrefixDelete (DotRef _ o fn) ->
              do f' <- primlowM $ StringLit def $ unId fn
                 o' <- rewrite o
                 PrefixExpr a PrefixDelete <$> deleteFieldM o' f'
            PrefixExpr a PrefixDelete (BracketRef _ o f) ->
              do f' <- rewrite f
                 o' <- rewrite o
                 PrefixExpr a PrefixDelete <$> deleteFieldM o' f'
            PrefixExpr a PrefixVoid se ->
              liftM (PrefixExpr a PrefixVoid) $ rewrite se
            PrefixExpr _ op expr -> rewrite expr >>= prefixOpM op
            -- compute the join of the levels of the operands
            InfixExpr a OpLAnd e1 e2 -> logicalOpCommon e1 e2 True
            InfixExpr a OpLOr e1 e2 -> logicalOpCommon e1 e2 False
            InfixExpr _ op e1 e2 -> do e1' <- rewrite e1
                                       e2' <- rewrite e2
                                       infixOpM op e1' e2'
            -- | Inserts No-Sensitive-Upgrade checks on assignments
            -- and other side-effectful constructs are inserted. The
            -- read/write operations are also modified in order to be
            -- aware of the "soft deletes". (I-ASSIGN)
            UnaryAssignExpr a op lhs ->
              case lhs of
                -- Since a reference error is going to be thrown for a
                -- non-existent variable, we don't check whether the
                -- var is declared or not.
                VarRef {} -> varUnaryAssignOpM op lhs
                DotRef _ obj fname -> do obj' <- rewrite obj
                                         field <- primlowM $ string $ unId fname
                                         fieldUnaryAssignOpM op obj' field
                BracketRef _ obj field -> do obj' <- rewrite obj
                                             field' <- rewrite field
                                             fieldUnaryAssignOpM op obj' field'
            AssignExpr a op lhs rhs -> rewrite rhs >>= \rhs' ->
              case lhs of
                VarRef a vn -> do
                  lhs2 <- if a^._2.transformAnn.varNotDeclared
                          then varRead $ unId vn
                          else return lhs
                  assignVarWithOp op lhs2 rhs'
                DotRef _ obj fname -> do obj' <- rewrite obj
                                         field <- primlowM $ string $ unId fname
                                         assignFieldWithOp op obj' field rhs'
                BracketRef _ obj field -> do obj' <- rewrite obj
                                             field' <- rewrite field
                                             assignFieldWithOp op obj' field' rhs'
            CommaExpr a se -> liftM (CommaExpr a) $ mapM rewrite se
    in exprCases >=> exprControlFlow

instance Rewritable PropAssign where
  rewrite pa = case pa of
    PValue a p e  -> liftM (PValue a p) $ rewrite e
    PGet a p body -> liftM (PGet a p) $ mapM rewrite body
    PSet a p id body -> liftM (PSet a p id) $ mapM rewrite body

instance Rewritable Statement where
  rewrite =
    let initVD (VarDecl a id minit) = liftM (VarDecl a id . Just) $ case minit of
          Just init -> case init of
            FuncExpr {}-> rewrite init
            _          -> error "initVD: unexpected non-function var initializer"
          Nothing -> initVarM
        mrewrite :: (Rewritable a, Default b)
                 => Maybe (a (b, Annotation))
                 -> Rewriter (Maybe (a (b, Annotation)))
        mrewrite = mapM rewrite
        stmtCases s = case s of
          IfStmt a g t e ->
            liftM3 (IfStmt a) (rewrite g >>= toBooleanV) (rewrite t) (rewrite e)
          SwitchStmt a g cs ->
            liftM2 (SwitchStmt a)
            (liftM valueof $ rewrite g)
            (mapM rewrite cs)
          WhileStmt a g b ->
            liftM2 (WhileStmt a)
            (rewrite g >>= toBooleanV)
            (rewrite b)
          DoWhileStmt a b g ->
            liftM2 (DoWhileStmt a) (rewrite b) $ rewrite g >>= toBooleanV
          ForInStmt a i o b -> do
            o' <- liftM valueof $ rewrite o >>= toObjectBoxM
            rtbl <- gettempM >>= primlowM
            wt   <- gettempM
            let i' = case i of
                  ForInVar (VarDecl a v _) -> VarDeclStmt def [VarDecl a v $ Just rtbl]
                  ForInExpr e -> expr $ e `assign` rtbl
            rb <- rewrite b
            return $ ForInStmt a (ForInExpr wt) o' $ i' `beforeS` rb
          ForStmt a i test inc b ->
            liftM4 (ForStmt a) (rewrite i)
            (mapM (rewrite >=> toBooleanV) test)
            (mrewrite inc) (rewrite b)
          TryStmt a body mCatch mFinally ->
            do id <- genUID
               newbody <- liftM2 (:) (remember id) (mapM rewrite body)
               case mFinally of
                  Just finally ->
                    liftM2 (TryStmt a newbody) (mrewrite mCatch)
                    (liftM Just $ liftM2 (:) (restore id) (mapM rewrite finally))
                  Nothing      ->
                    liftM2 (TryStmt a newbody) (mrewrite mCatch)
                    (liftM Just $ sequence [restore id])
          VarDeclStmt a vds -> liftM (VarDeclStmt a) (mapM initVD vds)
          BlockStmt a ss -> liftM (BlockStmt a) $ mapM rewrite ss
          EmptyStmt _   -> return s
          ExprStmt a e  -> liftM (ExprStmt a) $ rewrite e
          BreakStmt _ _ -> return s
          ContinueStmt _ _ -> return s
          LabelledStmt a l s -> liftM (LabelledStmt a l) $ rewrite s
          ThrowStmt a e -> liftM (ThrowStmt a) $ rewrite e
          ReturnStmt _ Nothing -> return s
          ReturnStmt a (Just e) -> liftM (ReturnStmt a . Just) $ rewrite e
          WithStmt a o s ->
            do s' <- rewrite s
               obj <- rewrite o >>= toObjectBoxM >>= adaptObjectM
               return $ WithStmt a obj s'
          FunctionStmt _ _ _ _ ->
            fail "Unexpected function statement in desugared code"
          DebuggerStmt _ -> return s
    in stmtCases >=> stmtControlFlow

instance Rewritable ForInit where
  rewrite i = case i of
    NoInit -> return NoInit
    VarInit _ -> fail "Unexpected var declaration in a for statement"
    ExprInit e -> liftM ExprInit $ rewrite e

instance Rewritable CatchClause where
  rewrite (CatchClause a x body) = liftM (CatchClause a x) $ mapM rewrite body

instance Rewritable CaseClause where
  rewrite c = case c of
    CaseClause a g b -> do g' <- rewrite g
                           liftM (CaseClause a (valueof g')) $ mapM rewrite b
    CaseDefault a body -> liftM (CaseDefault a) $ mapM rewrite body

-- | Insert 'push' and 'pop' into expressions based on the
-- annotations.
exprControlFlow :: Default a => Expression (a, Annotation) -> Rewriter (Expression (a, Annotation))
exprControlFlow e =
  let ta = e^.annot.transformAnn
      pushT = case ta^.push of
        NoPush -> return
        PushGuard i -> flip pushM $ int i
        PushException -> pushExceptionM
      afterPopT = maybe return (flip popM . int) $ ta^.afterPop
      beforePopT = maybe return
                   (\i x -> liftM (\p -> list [p,x]) $ popM null_ (int i))
                   (ta^.beforePop)
  in  afterPopT e >>= pushT >>= beforePopT

-- | Insert 'push' and 'pop' into statements based on the
-- annotation
stmtControlFlow :: Default a => Statement (a, Annotation) -> Rewriter (Statement (a, Annotation))
stmtControlFlow s =
  let ta = s^.annot.transformAnn
      beforePopT = maybe return
                   (\i x -> liftM (\p -> block [expr p,x]) $ popM null_ (int i))
                   (ta^.beforePop)
      afterPopT  = maybe return
                   (\i x -> liftM (\p -> block [x, expr p]) (popM null_ (int i)))
                   (ta^.afterPop)
  in beforePopT s >>= afterPopT
