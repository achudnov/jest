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


{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Inliner.Annotations where

import Data.Data (Data)
import Data.Typeable (Typeable)
import Language.ECMAScript5.Syntax
import Language.ECMAScript5.Syntax.Annotations
import Language.ECMAScript5.PrettyPrint
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Default.Class
import Language.ECMAScript5.Analysis.LabelSet
import Data.List (intercalate, elemIndex)
import Data.Maybe (catMaybes, fromJust)
import Data.Monoid
import Lens.Simple
import MyPrelude
import Data.Graph.Inductive.Graph (Node)
import Data.IntSet (IntSet)
import Control.Applicative
import Data.Int

data TransformationAnnotation = TransformationAnnotation {
  _lambdaSelfRef :: Bool -- ^ True means that the VarRef refers to an
                         -- enclosing named lambda (function
                         -- expression)
  ,_varNotDeclared :: Bool -- ^ True denotes a variable that wasn't
                           -- declared with a var-declaration (can't
                           -- determine statically if it was)
  ,_funcMayThrow :: Bool   -- ^ True denotes a function object that
                           -- may throw an exception. By default,
                           -- every function call is annotated with
                           -- it.
  ,_push :: Push -- ^ See `Push` 
  ,_beforePop :: Maybe Int32 -- ^ Pop the stack once before this node
  ,_afterPop :: Maybe Int32  -- ^ .. or after executing this
                             -- expression/statement
  }
  deriving (Data, Typeable)
           
-- `Push i`` means push a guard value with the IPD identified by
-- `i`. Can only be originate from 'after' nodes of expressions.
data Push = NoPush | PushGuard Int32 | PushException
  deriving (Data, Typeable)

instance Show Push where
  show p = case p of
    NoPush -> ""
    PushGuard i -> "Push " ++ show i
    PushException -> "PushException"

instance Default Push where
  def = NoPush

instance Monoid Push where
  mempty = def
  mappend p1 p2 = case p1 of
    NoPush -> p2
    _      -> p1

instance Default TransformationAnnotation where
  def = TransformationAnnotation False False False def Nothing Nothing

instance Show TransformationAnnotation where
  show ta = (if _lambdaSelfRef ta then "LambdaSelfRef, " else "")
          ++(if _varNotDeclared ta then "VarNotDeclared, " else "")
          ++(if _funcMayThrow ta then "FuncMayThrow, " else "")
          ++(show $ _push ta)
          ++(maybe "" (("BeforePop " ++) . show) $ _beforePop ta)
          ++(maybe "" (("AfterPop " ++) . show) $ _afterPop ta)

instance Monoid TransformationAnnotation where
  mempty = def
  mappend ta1 ta2 = TransformationAnnotation
    {_lambdaSelfRef = _lambdaSelfRef ta1 || _lambdaSelfRef ta2
    ,_varNotDeclared = _varNotDeclared ta1 || _varNotDeclared ta2
    ,_funcMayThrow = _funcMayThrow ta1 || _funcMayThrow ta2
    ,_push = mappend (_push ta1) (_push ta2)
    ,_beforePop = maybe (_beforePop ta2) (Just) $ _beforePop ta1
    ,_afterPop = maybe (_afterPop ta2) (Just) $ _afterPop ta1
    }

makeLenses ''TransformationAnnotation

-- | An annotation that tells which are the possible implicit
-- exceptions (native errors) that might be thrown by the current
-- construct and by the value of which expression they are
-- triggered. The following kinds of errors are supported: 
-- * ReferenceError 
-- * TypeError
-- 
-- RangeError, SyntaxError, URIError and EvalError are all raised in
-- built-in methods, so the static analysis does not take them into
-- account.

data NativeError = NativeError NativeErrorType (Expression ())
                 deriving (Eq, Ord, Data,Typeable)

instance Show NativeError where
  show (NativeError t e) = show t ++ " when " ++ (show $ prettyPrint e)

data NativeErrorType = ReferenceError | TypeError
                     deriving (Eq, Ord, Show, Data,Typeable)


data Annotation = 
  Annotation{_transformAnn :: TransformationAnnotation
            ,_nativeErrors :: Set NativeError
            ,_labelSet :: Set Label
            }
  deriving (Data, Typeable)
           
makeLenses ''Annotation

astAnnot :: (HasAnnotation x) => Lens (x a) (x a) a a
astAnnot = lens getAnnotation (flip setAnnotation)

-- | The inliner AST annotation lens
annot :: (HasAnnotation x) => Lens (x (a, Annotation)) (x (a, Annotation)) Annotation Annotation
annot = astAnnot._2

instance Show Annotation where
  show ann = 
    let f :: Show a => String -> Set a -> Maybe String
        f d x = if Set.null x then Nothing
                else Just $ d ++ (intercalate ", " $ map show (Set.toList x))
        pls ls = if Set.null ls || (Set.size ls == 1 && EmptyLabel `Set.member` ls) then
                   Nothing
                 else Just $ "Labels: " ++ (intercalate ", " $ map show $ filter (/=EmptyLabel) (Set.toList ls))
    in intercalate "\n" $
       ("Transformation annotations: " ++ show (_transformAnn ann)):
        catMaybes [f "Throws: " $ _nativeErrors ann
                  ,pls $ _labelSet ann
                  ]

instance Default (Annotation) where
  def = Annotation {_transformAnn = def
                   ,_nativeErrors = Set.empty
                   ,_labelSet     = Set.empty
                   }

instance Monoid Annotation where
  mappend a1 a2 = Annotation {_transformAnn = _transformAnn a1 <> _transformAnn a2
                             ,_nativeErrors = _nativeErrors a1 <> _nativeErrors a2
                             ,_labelSet     = _labelSet a1     <> _labelSet a2
                             }
  mempty = def
