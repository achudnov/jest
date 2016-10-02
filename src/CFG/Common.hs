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


{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, StandaloneDeriving, FlexibleInstances #-}

-- | Common types and functions for control-flow graphs

module CFG.Common where

import Language.ECMAScript5.Syntax
import Language.ECMAScript5.Syntax.Annotations
import Language.ECMAScript5.Analysis.LabelSet (Label (..))
import Data.Graph.Inductive
import Data.Set (Set)
import Data.IntSet (IntSet)
import Data.IntMap (IntMap)
import qualified Data.IntSet as S
import Lens.Simple
import Data.Default.Class
import Data.Data
import Data.Typeable (Typeable)
import Inliner.Annotations

data NodeLabel = Begin String
               | End String
               | NormalReturn
               | ExceptionalReturn
               | Ghost {- aka SEN -}
               deriving (Show, Eq)

data CFG = CFG {_graph :: Gr NodeLabel ()
               ,_startNode :: Node
               ,_normalReturnNode :: Node
               ,_exceptionalReturnNode :: Node}

instance Default CFG where
  def = CFG {_graph = mkGraph [] []
            ,_startNode = 0
            ,_normalReturnNode = 0
            ,_exceptionalReturnNode = 0
            }

makeLenses ''CFG

data CFAAnnot a = CFAAnnot {_inlinerAnnot :: Annotation
                           ,_initAnnot :: a
                           ,_cfgEntryNode :: Node
                           ,_cfgExitNode  :: Node
                           ,_cfgSyntaxLabel :: String
                           }
                  deriving (Data, Typeable)

instance Default a => Default (CFAAnnot a) where
  def = CFAAnnot def def 0 0 ""

makeLenses ''CFAAnnot

inlAn :: (HasAnnotation x) => Lens (x (CFAAnnot a)) (x (CFAAnnot a)) Annotation Annotation
inlAn = astAnnot.inlinerAnnot

entryNode :: (HasAnnotation x) => Lens (x (CFAAnnot a)) (x (CFAAnnot a)) Node Node
entryNode = astAnnot.cfgEntryNode

exitNode ::(HasAnnotation x) => Lens (x (CFAAnnot a)) (x (CFAAnnot a)) Node Node
exitNode = astAnnot.cfgExitNode
  
liftCFAAnnot :: (a, Annotation) -> CFAAnnot a
liftCFAAnnot (a, ann) = CFAAnnot {_inlinerAnnot = ann
                                 ,_initAnnot = a
                                 ,_cfgEntryNode = 0
                                 ,_cfgExitNode = 0
                                 ,_cfgSyntaxLabel = ""
                                 }

lowerCFAAnnot :: CFAAnnot a -> (a, Annotation)
lowerCFAAnnot cfaa = (cfaa^.initAnnot, cfaa^.inlinerAnnot)


type NodeSet = IntSet

-- | Control-dependence region tree
data CDR = CDR {_cdrEntry :: Node
               ,_cdrExits :: NodeSet
               ,_cdrGuards :: NodeSet
               ,_region :: NodeSet
               ,_junction :: Node
               ,_nested :: [CDR]
               }
         deriving (Data, Typeable, Show)

data Type = Folded 
          | Dependent NodeSet
           | Independent
          deriving (Eq, Show)

isDependent :: Type -> Bool
isDependent (Dependent _) = True
isDependent _             = False

isDependentOn :: Type -> Node -> Bool
isDependentOn (Dependent ns) n  = S.member n ns
isDependentOn _              _  = False

mkDependentOnSet :: NodeSet -> Type -> Type
mkDependentOnSet ns1 (Dependent ns2) = Dependent $ S.union ns1 ns2
mkDependentOnSet ns _                = Dependent ns


mkDependentOn :: Node -> Type -> Type
mkDependentOn n = mkDependentOnSet $ S.singleton n

mkFolded :: Type -> Type
mkFolded Independent = Folded
mkFolded a           = a

data IResult = IResult {_itype :: Type
                       ,_primaryGuard :: Node
                       ,_cdr   :: CDR}
               deriving (Show)

makeLenses ''CDR
makeLenses ''IResult


-- | Information about iteration and switch statements
data IterSwitch = Iter Node Node (Set Label)
                | Switch Node (Set Label)
                  deriving (Show, Eq)

targetForBreak :: IterSwitch -> Node
targetForBreak (Iter break _ _) = break
targetForBreak (Switch break _) = break


targetForContinue :: IterSwitch -> Maybe Node
targetForContinue (Iter _ continue _) = Just continue
targetForContinue _                 = Nothing

getIterSwitchLabelSet :: IterSwitch -> Set Label
getIterSwitchLabelSet (Iter _ _ ls) = ls
getIterSwitchLabelSet (Switch _ ls) = ls

data BuilderState = 
  BuilderState {_flowGraph :: CFG -- ^ current state of the
                                 -- control-flow graph
               ,_enclosingIterSwitchStmts :: [IterSwitch]
                                      -- ^ enclosing iteration and switch
                                      -- statements (True if Switch)
               ,_enclosingHandlers :: [Node]  -- ^ enclosing exception
                                             -- handlers
               }

makeLenses ''BuilderState
