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


{-# LANGUAGE TemplateHaskell #-}

module SyntaxHelpers where

import Language.ECMAScript5.Syntax
import qualified Language.ECMAScript5.Syntax.CodeGen as JS
import Language.ECMAScript5.Parser
import Language.ECMAScript5.Syntax.Annotations
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Data (Data)
import Data.Typeable (Typeable)
import Data.Generics.Uniplate.Data
import Data.Default.Class
import Data.Traversable (Traversable)
import Language.Haskell.TH
import Language.Haskell.TH.Lift
import Language.Haskell.TH.Syntax
import Data.Monoid
import SyntaxHelpers.JSFunc
import Control.Monad
import Control.Monad.Identity

newtype Fix a = Fix (a (Fix a))

type PrefixTree = Fix (Map Char)

isPrefixOf :: String -> PrefixTree -> Bool
isPrefixOf []     _  = True
isPrefixOf (c:cs) (Fix pt) = case Map.lookup c pt of
                              Nothing  -> False
                              Just pt2 -> isPrefixOf cs pt2

mkPrefixTree :: [String] -> PrefixTree
mkPrefixTree = (foldl combinePT (Fix Map.empty)) . (map mkPT)
  where mkPT [] = Fix Map.empty
        mkPT (c:cs) = Fix $ Map.singleton c $ mkPT cs
        combinePT (Fix pt1) (Fix pt2) = Fix $ Map.unionWith combinePT pt1 pt2
          

boundVariables :: (Data a) => Program a -> Set String
boundVariables js = 
  unicollectVars varDecl js `Set.union` 
  unicollectVars expr js `Set.union` 
  unicollectVars forInInit js
  where unicollectVars :: (Data a, Data (b a)) => 
                          (b a -> Set String) -> Program a -> Set String
        unicollectVars f js = foldr (Set.union . f) Set.empty 
                              (universeBi js)
        varDecl :: VarDecl a -> Set String
        varDecl (VarDecl _ id _) = Set.singleton $ unId id
        expr :: Expression a -> Set String
        expr (VarRef _ id) = Set.singleton $ unId id
        expr _             = Set.empty
        forInInit :: ForInInit a -> Set String
        forInInit (ForInVar (VarDecl _ id _)) = Set.singleton $ unId id
        forInInit _             = Set.empty

redef :: (Default b, Traversable x) => x a -> x b
redef = reannotate (const def)

deriveLiftMany [''Program, ''Statement, ''Expression, ''CaseClause, ''CatchClause, ''ForInit, ''ForInInit, ''VarDecl, ''InfixOp, ''AssignOp, ''Id, ''PrefixOp, ''Prop, ''UnaryAssignOp, ''JSFunc, ''PropAssign]

-- instance Lift Double where
--   lift = return . LitE . DoublePrimL . fromRational . toRational

instance Monoid SourcePos where
  mappend p _ = p
  mempty = def

instance Monoid SourceSpan where
  mappend s _ = s
  mempty = def

loadJS :: FilePath -> Q Exp
loadJS f = loadAndParse f >>= (return . removeAnnotations) >>= lift

loadAndParse :: FilePath -> Q (Program ParserAnnotation)
loadAndParse f = addDependentFile f >> runIO (parseFromFile f) 

hygienic :: Default a => [Statement a] -> Expression a -> Expression a
hygienic ss re = JS.call (JS.lambda [] $ ss ++ [JS.returns re]) []

transformBodiesM :: forall a m. (Data a, Typeable a, Monad m)
                 => ([Statement a] -> m [Statement a])
                 -> Program a -> m (Program a)
transformBodiesM f = transformBiM (transformFuncExprs f)
                  >=>transformBiM (transformFuncStmts f)
                  >=>(\(Program a ss)-> liftM (Program a) (f ss))
  where transformFuncExprs :: ([Statement a] -> m [Statement a])
                           -> Expression a -> m (Expression a)
        transformFuncExprs f e = case e of
          FuncExpr a mid args body -> liftM (FuncExpr a mid args) (f body)
          _ -> return e
        transformFuncStmts :: ([Statement a] -> m [Statement a])
                           -> Statement a -> m (Statement a)
        transformFuncStmts f e = case e of
          FunctionStmt a id args body -> liftM (FunctionStmt a id args) (f body)
          _ -> return e

transformBodies :: (Data a, Typeable a) => ([Statement a] -> [Statement a])
                -> Program a -> Program a
transformBodies f = runIdentity . transformBodiesM (return . f)

traverseAnnotationsWithinBodyM :: forall a x m.
                                 (HasAnnotation x, Monad m, Data (x a), Typeable (x a), Data a, Typeable a)
                              => (a -> m a) -> x a -> m (x a)
traverseAnnotationsWithinBodyM f = descendBiM stmt >=> descendBiM expr
                               >=> descendBiM cc   >=> descendBiM vd
                               >=> descendBiM catch>=> descendBiM ident
                               >=> descendBiM prop
  where ff :: (HasAnnotation y) => y a -> m (y a)
        ff x = liftM ((flip setAnnotation) x) (f (getAnnotation x))
        stmt :: Statement a -> m (Statement a)
        stmt = ff  >=> \s -> case s of
                              FunctionStmt {} -> return s
                              _ ->  descendM stmt s
                                    >>= descendBiM expr
                                    >>= descendBiM cc
                                    >>= descendBiM vd
                                    >>= descendBiM catch
        expr :: Expression a -> m (Expression a)
        expr = ff >=> \e -> case e of
                               FuncExpr {} -> return e
                               _ -> descendM expr e
                                    >>= descendBiM ident
                                    >>= descendBiM prop
        cc :: CaseClause a -> m (CaseClause a)
        cc = ff >=> descendBiM expr >=> descendBiM stmt >=> descendBiM ident
        vd :: VarDecl a -> m (VarDecl a)
        vd = ff >=> descendBiM expr >=> descendBiM ident
        catch :: CatchClause a -> m (CatchClause a)
        catch = ff >=> descendBiM stmt >=> descendBiM ident
        ident :: Id a -> m (Id a)
        ident = ff
        prop :: Prop a -> m (Prop a)
        prop = ff >=> descendBiM ident

traverseAnnotationsWithinBody :: (HasAnnotation x, Data (x a), Typeable (x a), Data a, Typeable a)
                              => (a -> a) -> x a -> x a
traverseAnnotationsWithinBody f = runIdentity . traverseAnnotationsWithinBodyM (return . f)
