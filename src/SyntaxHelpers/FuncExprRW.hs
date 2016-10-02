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


{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module SyntaxHelpers.FuncExprRW (funcExprRewriteM) where

import Language.ECMAScript5.Syntax
import Control.Arrow
import Data.Functor
import Control.Monad

funcExprRewriteM :: (Monad m, FuncExprRW m a)
                 => (Expression b -> m (Expression b)) -> a b -> m (a b)
funcExprRewriteM = rw

class Monad m => FuncExprRW m a where
  rw :: (Expression b -> m (Expression b)) -> a b -> m (a b)

instance Monad m => FuncExprRW m Program where
  rw f (Program a ss) = do nss <- mapM (rw f) ss
                           return $ Program a nss

instance Monad m => FuncExprRW m PropAssign where
  rw f (PValue a p e) = liftM (PValue a p) (rw f e)
  rw f (PGet a p body) = liftM (PGet a p) (mapM (rw f) body)
  rw f (PSet a p i body) = liftM (PSet a p i) (mapM (rw f) body)

instance Monad m => FuncExprRW m Expression where
  rw _ (StringLit a s)              = return $ StringLit a s
  rw _ r@(RegexpLit {})             = return r
  rw _ (NumLit a d)                 = return $ NumLit a d
  rw _ (BoolLit a b)                = return $ BoolLit a b
  rw _ (NullLit a)                  = return $ NullLit a
  rw f (ArrayLit a exps)            = do nexps <- mapM (maybeMapM $ rw f) exps
                                         return $ ArrayLit a nexps
  rw f (ObjectLit a props)          = 
    let g [] = return []
        g (pa:xs) = liftM2 ((:)) (rw f pa) (g xs)
    in do nprops <- g props   
          return $ ObjectLit a nprops
  rw _ (ThisRef a)                  = return $ ThisRef a
  rw _ (VarRef a id)                = return $ VarRef a id
  rw f (DotRef a exp id)            = do nexp <- rw f exp
                                         return $ DotRef a nexp id
  rw f (BracketRef a container key) = do ncontainer <- rw f container
                                         nkey <- rw f key
                                         return $ BracketRef a ncontainer nkey
  rw f (NewExpr a ctor params)      = do nctor <- rw f ctor
                                         nparams <- mapM (rw f) params
                                         return $ NewExpr a nctor nparams
  rw f (PrefixExpr a op e)          = do ne <- rw f e
                                         return $ PrefixExpr a op ne
  rw f (UnaryAssignExpr a op lv)    = do nlv <- rw f lv
                                         return $ UnaryAssignExpr a op nlv
  rw f (InfixExpr a op e1 e2)       = do ne1 <- rw f e1
                                         ne2 <- rw f e2
                                         return $ InfixExpr a op ne1 ne2
  rw f (CondExpr a g et ef)         = do net <- rw f et
                                         nef <- rw f ef
                                         return $ CondExpr a g net nef
  rw f (AssignExpr a op lv e)       = do nlv <- rw f lv
                                         ne <- rw f e
                                         return $ AssignExpr a op nlv ne
  rw f (CommaExpr a es)             = do nes <- mapM (rw f) es
                                         return $ CommaExpr a nes
  rw f (CallExpr a fn params)       = do nfn <- rw f fn
                                         nparams <- mapM (rw f) params
                                         return $ CallExpr a nfn nparams
  rw f (FuncExpr a mid args ss)      = do nss <- mapM (rw f) ss
                                          rw f (FuncExpr a mid args nss)

instance Monad m => FuncExprRW m Statement where
  rw f (BlockStmt a ss) = do nss <- mapM (rw f) ss
                             return $  BlockStmt a nss
  rw _ (EmptyStmt a)    = return $  EmptyStmt a
  rw f (ExprStmt a e)   = do ne <- rw f e
                             return $  ExprStmt a ne
  rw f (IfStmt a g th el) = do ng <- rw f g
                               nth <- rw f th
                               nel <- rw f el
                               return $  IfStmt a ng nth nel
  rw f (SwitchStmt a g cs)  = do ng <- rw f g
                                 ncs <- mapM (rw f) cs
                                 return $  SwitchStmt a ng ncs
  rw f (WhileStmt a g s)    = do ng <- rw f g
                                 ns <- rw f s
                                 return $  WhileStmt a ng ns
  rw f (DoWhileStmt a s g)  = do ng <- rw f g
                                 ns <- rw f s
                                 return $  DoWhileStmt a ns ng
  rw _ (BreakStmt a mid)    = return $  BreakStmt a mid
  rw _ (ContinueStmt a mid) = return $  ContinueStmt a mid
  rw f (LabelledStmt a id s) = do ns <- rw f s
                                  return $  LabelledStmt a id ns
  rw f (ForInStmt a i s1 s2) = do ns1 <- rw f s1
                                  ns2 <- rw f s2
                                  return $  ForInStmt a i ns1 ns2
  rw f (ForStmt a i mt mi s) = do ni <- rw f i
                                  nmt <- maybeMapM (rw f) mt
                                  nmi <- maybeMapM (rw f) mi
                                  ns <- rw f s
                                  return $ ForStmt a ni nmt nmi ns
  rw f (TryStmt a b mc mf)   = do nb <- mapM (rw f) b 
                                  nmc <- maybeMapM (rw f) mc
                                  nmf <- maybeMapM (mapM $ rw f) mf
                                  return $  TryStmt a nb nmc nmf
  rw f (ThrowStmt a e)       = do ne <- rw f e
                                  return $ ThrowStmt a ne
  rw f (ReturnStmt a me)     = do nme <- maybeMapM (rw f) me
                                  return $  ReturnStmt a nme
  rw f (WithStmt a e s)      = do ne <- rw f e
                                  ns <- rw f s
                                  return $  WithStmt a ne ns
  rw f (VarDeclStmt a vds)   = do nvds <- mapM (rw f) vds
                                  return $  VarDeclStmt a nvds
  rw f (FunctionStmt a id args ss) = do nss  <- mapM (rw f) ss
                                        return $ FunctionStmt a id args nss
  
instance Monad m => FuncExprRW m CaseClause where
  rw f (CaseClause a e ss) = do ne <- rw f e
                                nss <- mapM (rw f) ss
                                return $  CaseClause a ne nss
  rw f (CaseDefault a ss)  = do nss <- mapM (rw f) ss
                                return $ CaseDefault a nss

instance Monad m => FuncExprRW m ForInit where
  rw f NoInit        = return NoInit
  rw f (VarInit vds) = do nvds <- mapM (rw f) vds
                          return $ VarInit nvds
  rw f (ExprInit e)  = do ne <- rw f e
                          return $ ExprInit ne

instance Monad m => FuncExprRW m CatchClause where
  rw f (CatchClause a id s) = do ns <- mapM (rw f) s
                                 return $ CatchClause a id ns

instance Monad m => FuncExprRW m VarDecl where
  rw f (VarDecl a id me) = do nme <- maybeMapM (rw f) me
                              return $ VarDecl a id nme
                                    
maybeMapM :: Monad m => (a -> m b) -> Maybe a -> m (Maybe b)
maybeMapM f (Just a) = do b <- f a
                          return $ Just b
maybeMapM f Nothing  = return Nothing
