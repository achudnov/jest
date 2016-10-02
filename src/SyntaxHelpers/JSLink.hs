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


{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A very simple JavaScript linker. Doesn't do any fancy checks and
-- would only be sound for well-behaved (e.g. strict) code that
-- doesn't use dynamic code evaluation or with.
module SyntaxHelpers.JSLink (link, getTopDecls, DeclKind (..), Decls  (..), renameDecls) where

import Language.ECMAScript5.Syntax
import Data.Generics.Uniplate.Data
import Data.Data (Data)
import Data.Typeable (Typeable)
import Control.Monad.Except
import Data.Maybe
import MyPrelude
import Data.Monoid
import Data.Map (Map)
import qualified Data.Map as Map

link :: (Data a, Typeable a, Monoid a) => (String -> String) -> [Program a] -> Program a
link renamer scripts =
  let decls = Map.unions $ map getTopDecls scripts
  in  mconcat $ map (link2 renamer decls) scripts


instance Monoid a => Monoid (Program a) where
  mappend (Program a1 body1) (Program a2 body2) = Program (a1 <> a2) (body1 <> body2)
  mempty = Program mempty []

link2 :: forall a. (Data a, Typeable a) => (String -> String) -> Decls -> Program a -> Program a
link2 renamer decls = transformBi te
  where te :: Expression a -> Expression a
        te e = case e of
          VarRef a1 (Id a2 n) ->
            case Map.lookup n decls of
             Nothing -> e
             Just _  -> VarRef a1 $ Id a2 $ renamer n
          _ -> e

renameDecls :: (String -> String) -> Program a -> Program a
renameDecls renamer (Program a ss) = Program a $ map rename ss
  where rename s = case s of
                    VarDeclStmt a vds -> VarDeclStmt a $ map rnVD vds
                    FunctionStmt a (Id a2 n) params body ->
                      FunctionStmt a (Id a2 $ renamer n) params body
                    _ -> s
        rnVD (VarDecl a (Id a2 n) me) = VarDecl a (Id a2 $ renamer n) me
          
getTopDecls :: Program a -> Decls
getTopDecls = Map.unions . map extractDecl . unProgram

extractDecl :: Statement a -> Decls
extractDecl s =
  let edvd (VarDecl _ (Id _ name) me) =
        Map.singleton name $ if isJust me then Const else Var
  in case s of
      VarDeclStmt _ vds -> Map.unions $ map edvd vds
      FunctionStmt _ name params _ -> Map.singleton (unId name) $ Function $ map unId params
      _ -> Map.empty

data DeclKind = Function [String] {- parameters -}
              | Var
              | Const

type Decls = Map String DeclKind

data LinkError = WrongNumberOfArguments String Int Int | ConstantAssigned String
               | FunctionAssigned String | ReadFromFunction String
               deriving Show
