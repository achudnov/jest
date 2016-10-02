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


{-# LANGUAGE FlexibleContexts #-}
-- | Static analyses needed for program rewriting. The results of the
-- analyses are recorded in AST annotations.

module Inliner.Analysis (analyse, initAnnotations, nativeErrorAnalysis, labelSets) where

import CFG.Common
import CFG.Construct
import CFG.ControlFlow
import CFG.Flatten
import Language.ECMAScript5.Syntax
import Language.ECMAScript5.Syntax.Annotations
import Language.ECMAScript5.Analysis.LabelSet
import Inliner.Annotations
import Inliner.Monad
import Data.Default.Class
import MyPrelude
import SyntaxHelpers
import Data.Data (Data)
import Data.Typeable (Typeable)
import Data.Traversable (Traversable)
import Data.Generics.Uniplate.Data
import Data.Set (Set)
import qualified Data.Set as Set
import Inliner.Annotations
import Control.Applicative ((<$>))
import Control.Arrow
import Control.Monad.Reader
import qualified Data.IntMap as IntMap
import Data.Maybe (maybeToList, isJust)
import Lens.Simple

-- | Performs the various analyses required by the
-- transformations. The results of the analyses get recorded in the
-- annotations. Property: changes only the annotations, so that
-- \forall x. (removeAnnotations x) == (removeAnnotations $ analyse x)
analyse :: (Data a, Default a)
        => Program a -> Program (a, Annotation)
analyse = controlFlow
        . markUndeclaredVars
        . markFuncExprSelfRefs
        . labelSets
        . nativeErrorAnalysis
        . initAnnotations

-- | Reannotates all the AST productions to allocate the inliner
-- annotations (to be filled in the later phases)
initAnnotations :: (Traversable t) => t a -> t (a, Annotation)
initAnnotations = reannotate $ \a -> (a, def)

-- | Annotates the Expression nodes that might throw a native error at
-- run-time with the type of the error and the condition under which
-- it will be thrown. NOTE: does not include the ReferenceErrors due
-- to improper targets of assignment as the current AST precludes from
-- that. A coarse and rudimentary exception analysis for
-- Program. Annotates each syntax node with a list of exceptions
-- that it might throw.
nativeErrorAnalysis :: forall a. (Data a, Typeable a) => 
                       Program (a, Annotation) 
                    -> Program (a, Annotation)
nativeErrorAnalysis = transformBi annotateNativeErrorsVD 
                    . transformBi annotateNativeErrorsS
                    . transformBi annotateNativeErrorsE
  where annotateNativeErrorsE :: Expression (a, Annotation) 
                              -> Expression (a, Annotation)
        annotateNativeErrorsE e = case e of
          ArrayLit a elems -> ArrayLit a (map (fmap getValue) elems)
          ObjectLit a inits -> ObjectLit a (map annotateNativeErrorsP inits)
          DotRef a obj id -> DotRef a (toObject $ getValue obj) id
          BracketRef a obj field -> 
            BracketRef a (toObject $ getValue obj) (getValue field)
          NewExpr a ctor args -> NewExpr a (typeError $ getValue ctor)
                                           (map getValue args)
          -- | Type errors due to a non-function argument are already
          -- handled in invokeCommon
          -- CallExpr a fun args -> CallExpr a (typeError $ getValue fun)
          --                                   (map getValue args)
          UnaryAssignExpr a op e -> UnaryAssignExpr a op (getValue e) 
          PrefixExpr a op e | op `notElem` [PrefixDelete, PrefixTypeof] -> 
              PrefixExpr a op (getValue e)
          InfixExpr a op e1 e2 -> InfixExpr a op (getValue e1) (getValue e2)
          CondExpr a g e1 e2 -> CondExpr a (getValue g) (getValue e1) (getValue e2)
          CommaExpr a es -> CommaExpr a (map getValue es)
          AssignExpr a OpAssign lhs rhs  ->
            AssignExpr a OpAssign lhs (getValue rhs)
          AssignExpr a op lhs rhs  ->  
            AssignExpr a op (getValue lhs) (getValue rhs)
          _ -> e
        annotateNativeErrorsS :: Statement (a, Annotation)
                              -> Statement (a, Annotation)
        annotateNativeErrorsS s =  case s of
          ExprStmt a e -> ExprStmt a (getValue e)
          IfStmt a g t e -> IfStmt a (getValue g) t e
          WhileStmt a g b -> WhileStmt a (getValue g) b
          DoWhileStmt a b g -> DoWhileStmt a b (getValue g)
          ForInStmt a fiv@(ForInVar _) obj body ->
            ForInStmt a fiv (toObject $ getValue obj) body
          ForInStmt a (ForInExpr e) obj body ->
            ForInStmt a (ForInExpr $ getValue e) (toObject $ getValue obj) body
          ForStmt a (ExprInit e) mtest minc body ->  
            ForStmt a (ExprInit $ getValue e) (getValue <$> mtest)
                      (getValue <$> minc) body
          ForStmt a init mtest minc body ->  
            ForStmt a init (getValue <$> mtest) (getValue <$> minc) body
          ReturnStmt a me -> ReturnStmt a (getValue <$> me)
          WithStmt a obj body -> WithStmt a (toObject $ getValue obj) body
          SwitchStmt a g cs -> SwitchStmt a (getValue g)
                               (map annotateNativeErrorsCase cs)
          _ -> s
        annotateNativeErrorsVD :: VarDecl (a, Annotation)  
                               -> VarDecl (a, Annotation)
        annotateNativeErrorsVD (VarDecl a id mexp) = 
          VarDecl a id (getValue <$> mexp)
        annotateNativeErrorsCase :: CaseClause (a, Annotation)
                                 -> CaseClause (a, Annotation)   
        annotateNativeErrorsCase c = case c of
          CaseClause a g body -> CaseClause a (getValue g) body
          _ -> c
        annotateNativeErrorsP :: PropAssign (a, Annotation)
                              -> PropAssign (a, Annotation)
        annotateNativeErrorsP p = case p of
          PValue a p e   -> PValue a p $ getValue e
          PGet a p ss    -> PGet a p $ map annotateNativeErrorsS ss
          PSet a p id ss -> PSet a p id $ map annotateNativeErrorsS ss

addNativeError :: NativeError -> Annotation -> Annotation
addNativeError ne ann = ann&nativeErrors %~ Set.insert ne

addNativeErrors :: [NativeError] -> Annotation -> Annotation
addNativeErrors nerrs ia = foldl (flip addNativeError) ia nerrs

-- | If getValue, when passed the evaluated expression, would throw a
-- ReferenceError, this function annotates this expression appropriately
getValue :: Expression (a, Annotation)
         -> Expression (a, Annotation)
getValue e = case e of
  -- The fact whether the variable exists or not does not depend on
  -- the variable value,
  -- VarRef _ _ -> referenceError e
  DotRef a obj id | canBeNull obj -> DotRef a (referenceError obj) id
  BracketRef a obj field | canBeNull obj -> BracketRef a (referenceError obj) field
  _ -> e

-- | A generic, unconditional reference error
referenceError :: Expression (a, Annotation) 
               -> Expression (a, Annotation)
referenceError e = e&annotation %~ (second (addNativeError $ 
                                            NativeError ReferenceError (removeAnnotations e)))
                   
-- | If toObject, when passed the evaluated expression, would throw a
-- TypeError, this function annotates this expression appropriately
toObject :: Expression (a, Annotation)
         -> Expression (a, Annotation)
toObject e | canBeUndefined e = typeError e
toObject e                    = e

-- | An unconditional TypeReference error when the condition is hard
-- to formalize or check statically (cf. NewExpr)
typeError :: Expression (a, Annotation)
          -> Expression (a, Annotation)
typeError e = e&annotation %~ (second (addNativeError $
                                       NativeError TypeError (removeAnnotations e)))

-- | returns true iff an expression can evaluate to @null@
canBeNull :: Expression a -> Bool
canBeNull e = case e of
  NullLit _ -> True
  DotRef {} -> True
  BracketRef {} -> True
  VarRef  {} -> True
  CallExpr {} -> True
  _         -> False
  
-- | returns true iff an expression can evaluate to @undefined@
canBeUndefined :: Expression a -> Bool
canBeUndefined e = case e of
  VarRef {} -> True
  DotRef {} -> True
  BracketRef {} -> True
  PrefixExpr _ PrefixVoid _ -> True
  InfixExpr _ op _ _ -> isComparisonOp op
  CallExpr {} -> True
  _ -> False
  
-- | Returns true if the operator is one of: "<", ">", "<=", ">="
isComparisonOp :: InfixOp -> Bool
isComparisonOp op = case op of
  OpLT -> True
  OpLEq -> True
  OpGT -> True
  OpGEq -> True
  _     -> False

-- | A wrapper around language-ecmascript 'annotateLabelSets'
labelSets :: Data a => Program (a, Annotation) -> (Program (a, Annotation))
labelSets = annotateLabelSets (^._2.labelSet) (\ls a -> a&_2.labelSet .~ ls)

-- | mark VarRef's that refer to the name of an enclosing named
-- FuncExpr.
markFuncExprSelfRefs :: forall a. Data a => Program (a, Annotation)
                     -> Program (a, Annotation)
markFuncExprSelfRefs = descendBi (funcExpr [])
  -- 1. find all the function expressions
  -- 2. descend into their bodies and find
  where funcExpr :: [String] -> Expression (a, Annotation) -> Expression (a, Annotation)
        funcExpr names e = case e of
          FuncExpr a mid pars body ->
            let newnames = case mid of
                  Just ident -> unId ident:names
                  Nothing    -> names
                newbody = map (descendBi (funcExpr newnames) . markStmts newnames) body
            in FuncExpr a mid pars newbody
          _ -> descend (funcExpr names) e
        markStmts :: [String] -> Statement (a, Annotation) -> Statement (a, Annotation)
        markStmts names = descendBi (markExprs names)
        markExprs :: [String] -> Expression (a, Annotation) -> Expression (a, Annotation)
        markExprs names = descend (markExprs names) . (annotateFunRef names)
        annotateFunRef :: [String] -> Expression (a, Annotation) -> Expression (a, Annotation)
        annotateFunRef names e = case e of
          VarRef _ ident | unId ident `elem` names ->
                           e&annot.transformAnn.lambdaSelfRef .~ True
          _ -> e

-- | Marks variables that are targets of assignments and have not been
-- declared previously (using a variable declaration, an ordinary
-- asssignment or by being in the run-time environment).
markUndeclaredVars :: forall a. Data a => Program (a, Annotation)
                   -> Program (a, Annotation)
markUndeclaredVars (Program a body) = 
  let declaredVars = Set.unions . map varNames
      varNames s   = case s of
        VarDeclStmt _ vds -> Set.unions (map varName vds)
        _                 -> Set.empty
      varName (VarDecl _ id _) = Set.singleton $ unId id
      markVars :: MonadReader (Set String) m =>
                  [Statement (a, Annotation)] -> m [Statement (a, Annotation)]
      markVars ss = local (`Set.union` declaredVars ss) $ mapM markVarsS ss
      -- For every nested expression, not crossing function
      -- boundaries, look at every variable reference and mark it as
      -- VarNotDeclared if it does not occur in the current lexical
      -- environment (the Reader environment)
      markVarsS s = descendBiM markVarsE s
        -- FunctionStmt {} -> error "Unexpected Function statement in desugared code"
        -- VarDeclStmt {} ->
        --   do scope <- ask
        --      return $ s&annot.transformAnn %~ Set.insert (Scope scope)
      markVarsE :: MonadReader (Set String) m
                => Expression (a, Annotation) -> m (Expression (a, Annotation))
      markVarsE e = case e of
        VarRef _ id -> markIfNotDeclared (unId id) e
        FuncExpr a mid params body ->
          local (Set.union $ Set.fromList $ map unId $ params ++ maybeToList mid) $
          liftM (FuncExpr a mid params) $ markVars body
        _ -> descendM markVarsE e
      markIfNotDeclared :: (HasAnnotation x, MonadReader (Set String) m) 
                        => String -> x (a, Annotation) -> m (x (a, Annotation))
      markIfNotDeclared vn x = varDeclared vn >>= \vd ->
        return (if not vd then x&annot.transformAnn.varNotDeclared .~ True
                else x)
      varDeclared :: MonadReader (Set String) m => String -> m Bool
      varDeclared name = reader (Set.member name)
  in  Program a (markVars body Set.empty)             
             
          
        
