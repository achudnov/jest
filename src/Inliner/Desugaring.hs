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
module Inliner.Desugaring (desugar) where

import Language.ECMAScript5.Syntax
import Language.ECMAScript5.Syntax.CodeGen
import Data.Data (Data)
import Data.Generics.Uniplate.Data
import Data.Default.Class
import Data.Maybe (catMaybes)
import Control.Arrow
import SyntaxHelpers
import Control.Monad.Writer
import MyPrelude
import Control.Applicative
import Data.Traversable (sequenceA)
import Data.List

-- | Performs some light desugaring transformations, that yield an
-- equivalent program. Primarily to simplify and make possible
-- rewriting transformations later.
desugar :: forall a. (Data a, Default a)
        => Program a
        -> Program a
desugar =
  transformBi dsFuncExprBodies . transformBi dsFuncStmtBodies . dsTopLevelScript
  where dsFuncExprBodies :: (Default a, Data a) => Expression a -> Expression a
        dsFuncExprBodies e = case e of
          FuncExpr a mid args body -> FuncExpr a mid args (dsBody body)
          _ -> e
        dsFuncStmtBodies :: (Default a, Data a) => Statement a -> Statement a
        dsFuncStmtBodies s = case s of
          FunctionStmt a name args body ->
            FunctionStmt a name args (dsBody body)
          _ -> s

data ExtractorState a = ES [VarDecl a] [VarDecl a]

instance Monoid (ExtractorState a) where
  mempty = ES [] []
  mappend (ES vdfl vdvl) (ES vdfr vdvr) = ES (vdfl ++ vdfr) (vdvl ++ vdvr)

dsTopLevelScript :: (Default a, Data a) => Program a -> Program a
dsTopLevelScript (Program a body) = Program a $ dsBody body

dsBody :: (Data a, Default a) => [Statement a] -> [Statement a]
dsBody ss = let dsEss = map dsExpressions ss
                (rmVdss, ES funDecls varDecls) = runWriter $ mapM extract dsEss
                decls = case mergeDecls funDecls varDecls of
                  []  -> []
                  vds -> [VarDeclStmt def vds]
            in decls ++ rmVdss

dsExpressions :: forall a. (Data a, Default a) => Statement a -> Statement a
dsExpressions = id

-- | Traverses statements, not crossing function boundaries, removing
-- variable declarations and function statement, converting the latter
-- into variable declarations initialized with function expressions,
-- and remembering the order of said declarations separately. Note:
-- this is only sound if the order of resulting var decls reflects the
-- variable instantiation algorithm in spec 10.5: i.e. the function
-- declarations come before ordinary var declarations.

extract :: (Default a) => Statement a
        -> Writer (ExtractorState a) (Statement a)
extract s =
  let processClause c = case c of
        CaseClause a g body -> liftM (CaseClause a g) $ mapM extract body
        CaseDefault a body  -> liftM (CaseDefault a)  $ mapM extract body
      processCatch (Just (CatchClause a id body)) =
        liftM (Just . CatchClause a id) $ mapM extract body
      processCatch Nothing = return Nothing
  in case s of
    FunctionStmt a id args body ->
      rmFunStmt a id args body >> return (EmptyStmt def)
    BlockStmt a body -> liftM (BlockStmt a) $ mapM extract body
    IfStmt a g t e -> liftM2 (IfStmt a g) (extract t) (extract e)
    SwitchStmt a g clauses -> liftM (SwitchStmt a g) (mapM processClause clauses)
    WhileStmt a g body -> liftM (WhileStmt a g) (extract body)
    DoWhileStmt a body g -> extract body >>=
                            \b -> return (DoWhileStmt a b g)
    LabelledStmt a lab s -> liftM (LabelledStmt a lab) (extract s)
    ForInStmt a init o body ->
      case init of
       ForInVar (VarDecl a id Nothing) ->
         pushVarDecl (VarDecl def id Nothing) >>
         liftM (ForInStmt a (ForInExpr (VarRef a id)) o)
         (extract body)
       _           -> liftM (ForInStmt a init o) $ extract body

    ForStmt a init mtest minc body ->
      do newInit <- case init of
                     VarInit vds -> mapM rmVarDecl vds >>=
                                    \es -> return $
                                           case catMaybes es of
                                            [] -> NoInit
                                            es'-> ExprInit $ CommaExpr def es'
                     _ -> return init
         liftM (ForStmt a newInit mtest minc) $ extract body
    TryStmt a body mcatch mfinally -> liftM3 (TryStmt a) (mapM extract body)
                                      (processCatch mcatch)
                                      (sequenceA $ (mapM extract) <$> mfinally)
    WithStmt a o body -> liftM (WithStmt a o) (extract body)
    VarDeclStmt a vds -> liftM (catExpressions . catMaybes) $ mapM rmVarDecl vds
    _ -> return s

catExpressions :: (Default a) => [Expression a] -> Statement a
catExpressions [] = EmptyStmt def
catExpressions es = ExprStmt def $ CommaExpr def es

rmVarDecl :: (Default a) => VarDecl a
          -> Writer (ExtractorState a) (Maybe (Expression a))
rmVarDecl (VarDecl a id mexpr) =
  do pushVarDecl (VarDecl a id Nothing)
     return $ liftM (AssignExpr def OpAssign (VarRef def id)) mexpr

rmFunStmt :: Default a => a -> Id a -> [Id a] -> [Statement a] -> Writer (ExtractorState a) ()
rmFunStmt a name args body =
  pushFunDecl $ VarDecl a name $ Just $ FuncExpr def Nothing args body

pushFunDecl :: VarDecl a -> Writer (ExtractorState a) ()
pushFunDecl vd = tell $ ES [vd] []
pushVarDecl :: VarDecl a -> Writer (ExtractorState a) ()
pushVarDecl vd = tell $ ES [] [vd]

-- | Merges two lists of variable declarations, giving priority to the
-- first one
mergeDecls :: [VarDecl a] -> [VarDecl a] -> [VarDecl a]
mergeDecls = unionBy vdNameEq
  where getVDName (VarDecl _ (Id _ n) _) = n
        vdNameEq vd1 vd2 = getVDName vd1 == getVDName vd2




