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
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverlappingInstances #-}

-- | Construction of intra-procedural control-flow graphs for
-- JavaScript programs.
module CFG.Construct (constructCFG) where

import CFG.Common
import Data.Graph.Inductive hiding (Edge, (&))
import Data.Graph.Inductive.Query.DFS (reachable)
import Language.ECMAScript5.Syntax
import Inliner.Annotations
import MyPrelude
import SyntaxHelpers
import Control.Monad.State hiding (mapM, sequence)
import Data.Maybe
import Control.Applicative hiding (empty)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Language.ECMAScript5.Syntax.Annotations
import Language.ECMAScript5.Analysis.LabelSet (Label (..))
import Control.Arrow (first,second)
import Lens.Simple
import Data.Default.Class
import Data.Traversable
import Prelude hiding (mapM,sequence)
import Data.Data
import Data.Typeable (Typeable)
import Data.Generics.Uniplate.Data

{- Note [Control-Flow Graph Structure]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Due to non-syntactic control-flow (break/continue, exceptions) in
ECMAScript, we have to do control-flow analysis on control-flow
graphs. JEST Control-flow graphs are what we call "overlay graphs",
meaning that we don't use an intermediate representation suitable for
being used as nodes of a CFG. Rather, we reuse AST nodes, associating
control-flow graph nodes with (almost) all of them. The association is
done via unique numeric CFG node ID's stored in the AST
annotation. ID's start with 1.

CFG's are intraprocedural. This means there is a CFG per function
body, plus one for the top-level program. Functions and the top level
'Script' are annotated with their CFG's. In order to associate AST
nodes with graph nodes, we annotate AST nodes with graph node ID's.

To facilitate sound and precise control-flow analysis, every graph
includes dedicated phantom nodes: NormalReturn and ExceptionalReturn
--- that don't correspond to any AST node. All return statements as
well as the last statement in the body transfer control to
NormalReturn. All unhandled exception throws lead to the
ExceptionalReturn.
-}

constructCFG :: (Data a, Typeable a) =>
                [Statement (CFAAnnot a)] -> ([Statement (CFAAnnot a)], CFG)
constructCFG ss =   -- algorithm:
  let -- 1. label AST nodes with unique id's, not crossing function boundaries
      (iss, (nodes, nextNode)) = runState (mapM assignNodes ss) ([], 1)
      -- 2. create a graph with nodes matching unique id's on AST
      -- nodes.
      cfg0 = allocateNodes nodes nextNode
      -- 3. walk through the AST, setting up the edges
      (ss', cfg1) = build (connectList iss <* normalReturnFlow (maybeLast iss)) cfg0
      -- 4. Adjust the entry node
      cfg2 = cfg1&startNode .~ fromMaybe nextNode (listEntryNode iss)
      -- 5. Prune the unreachable nodes
      cfgFinal = prune cfg2
  in  (ss', cfgFinal)

-- | creates a control-flow graph with with node id's matching
-- the labels of syntactic productions. Allocates two extra nodes for NormalReturn and EexceptionalReturn
allocateNodes :: [(Node, NodeLabel)] -> Node -> CFG
allocateNodes nodes nextNode = 
  let cfgFirstNode = 1
      nrn = nextNode
      ern = nextNode + 1
  in  CFG {_graph = mkGraph (nodes ++ [(nrn, NormalReturn)
                                      ,(ern, ExceptionalReturn)]) []
          ,_startNode = cfgFirstNode
          ,_normalReturnNode = nrn
          ,_exceptionalReturnNode = ern
          }

type IDAssigner x a = x (CFAAnnot a) -> State ([(Node, NodeLabel)], Node) (x (CFAAnnot a))

-- | Assign sequential integer ID's and store them in the cfgNode
-- annotation. Do so not crossing function boundaries. Return the list
-- of ID's assigned together with node labels
assignNodes :: forall a. (Data a, Typeable a) => IDAssigner Statement a 
assignNodes =
  let initLabels :: Statement (CFAAnnot a) -> Statement (CFAAnnot a)
      initLabels = transformBi expr . transform stmt . transformBi vd . transformBi cc
      expr :: Expression (CFAAnnot a) -> Expression (CFAAnnot a)
      expr e = e&astAnnot.cfgSyntaxLabel .~ label e
      stmt :: Statement (CFAAnnot a) -> Statement (CFAAnnot a)
      stmt e = e&astAnnot.cfgSyntaxLabel .~ label e
      vd :: VarDecl (CFAAnnot a) -> VarDecl (CFAAnnot a)
      vd e = e&astAnnot.cfgSyntaxLabel .~ label e
      cc :: CatchClause (CFAAnnot a) -> CatchClause (CFAAnnot a)
      cc e = e&astAnnot.cfgSyntaxLabel .~ label e
  in traverseAnnotationsWithinBodyM
     (\a -> do entry <- use _2
               let exit = entry + 1
               _2 %= (+2)
               _1 %= ([(exit, End $ a^.cfgSyntaxLabel)
                      ,(entry, Begin $ a^.cfgSyntaxLabel)] ++)
               return (a & cfgEntryNode .~ entry
                       & cfgExitNode .~ exit))
     . initLabels


-- | a monad for setting up edges in the graph
type Builder a = State BuilderState a
                    
-- | The starting state for the graph builder
startingState :: CFG -> BuilderState
startingState cfg = BuilderState cfg [] []

-- | Builder runner
build :: Builder a -> CFG -> (a, CFG)
build x cfg = (runState x (startingState cfg))&_2 %~ view flowGraph

-- | Removes edges that can never be reached in program execution (for
-- now, only edges emanating from nodes inaccessible from the entry
-- point)
prune :: CFG -> CFG
prune cfg = cfg&graph .~ pgr
  where start = cfg^.startNode
        pgr = nfilter (flip Set.member reachableNodes) $ cfg^.graph
        reachableNodes = Set.fromList $ reachable (cfg^.startNode) (cfg^.graph)

{- Utilities -}

modifyGraph :: (Gr NodeLabel () -> Gr NodeLabel ()) -> Builder ()
modifyGraph = (flowGraph.graph %=)
     
-- | A class to unify functions for linking different entities (graph
-- nodes and trees)
class Edge a where
  edge :: a -> Builder ()

instance Edge (Node, Node) where
  edge (src, dst) = modifyGraph $ insEdge (src, dst, ())

instance Edge ([Node], Node) where
  edge ([], _)   = return ()
  edge (s:srcs, dst) = edge (s, dst) >> edge (srcs, dst)

instance (ControlFlowNode x, ControlFlowNode y) => 
         Edge (x (CFAAnnot a), y (CFAAnnot a)) where
  edge (x, y) = edge (x^.exitNode, y^.entryNode)           

instance (ControlFlowNode x, Edge (Node, y)) => Edge (x (CFAAnnot a), y) where
  edge (x, dst) = edge (x^.exitNode, dst)

instance (ControlFlowNode y, Edge (x, Node)) => Edge (x, y (CFAAnnot a)) where
  edge (src, y) = edge (src, y^.entryNode)
  

instance Edge (a, b) => Edge (Maybe a, Maybe b) where
  edge (Just a, Just b)= edge (a, b)
  edge _ = return ()

instance Edge (a, Node) => Edge (Maybe a, Node) where
  edge (Nothing, _) = return ()
  edge (Just a, b)= edge (a, b)
  
instance Edge (Node, b) => Edge (Node, Maybe b) where
  edge (_, Nothing) = return ()
  edge (a, Just b)= edge (a, b)


flow :: Edge (x, y) => x -> y -> Builder ()
flow = curry edge

withBreakTarget :: (Maybe Node -> Builder a) -> Builder a
withBreakTarget f = 
  let filtr _ = True
      action mi = f $ targetForBreak <$> mi
  in  withFirstEnclosingIterSwitchStmtWithFilter filtr action

withLabelledBreakTarget :: Id (CFAAnnot a) -> (Maybe Node -> Builder b) -> Builder b
withLabelledBreakTarget lab f = 
  let filtr is = (Label $ unId lab) `Set.member` getIterSwitchLabelSet is
      action mi = f $ targetForBreak <$> mi
  in  withFirstEnclosingIterSwitchStmtWithFilter filtr action

withContinueTarget :: (Maybe Node -> Builder a) -> Builder a
withContinueTarget f = 
  let filtr (Iter {}) = True
      filtr _            = False
      action mi = f $ mi >>= targetForContinue
  in  withFirstEnclosingIterSwitchStmtWithFilter filtr action

withLabelledContinueTarget :: Id (CFAAnnot a) -> (Maybe Node -> Builder b) -> Builder b
withLabelledContinueTarget lab f =
  let filtr is@(Iter {}) = (Label $ unId lab) `Set.member`
                           getIterSwitchLabelSet is
      filtr _            = False
      action mi = f $ mi >>= targetForContinue
  in  withFirstEnclosingIterSwitchStmtWithFilter filtr action

withFirstEnclosingIterSwitchStmtWithFilter :: (IterSwitch -> Bool)
                                           -> (Maybe IterSwitch -> Builder a)   
                                           -> Builder a
withFirstEnclosingIterSwitchStmtWithFilter filtr action = 
  do is <- use enclosingIterSwitchStmts
     action $ maybeHead $ filter filtr is

-- |Pushes an enclosing iteration or switch statement
pushIterSwitch :: IterSwitch -> Builder ()
pushIterSwitch is = enclosingIterSwitchStmts %= (is:)

popIterSwitch :: Builder ()
popIterSwitch = enclosingIterSwitchStmts %= safeTail

bracketIterSwitch :: IterSwitch -> Builder a -> Builder a
bracketIterSwitch is b = pushIterSwitch is *> b <* popIterSwitch

pushExceptionHandler :: Node -> Builder ()
pushExceptionHandler n = enclosingHandlers %= (n:)
      
popExceptionHandler :: Builder ()
popExceptionHandler = enclosingHandlers %= safeTail

bracketExceptionHandler :: Node -> Builder a -> Builder a
bracketExceptionHandler n b = pushExceptionHandler n *> b <* popExceptionHandler

{- Control-flow nodes -- the primary abstraction for building the graph -}

-- | A typeclass for most of the AST productions that are taken into
-- account in the control-flow graph. Each tree node is linked to the
-- corresponding graph nodes by the means of two unique id.  The nodes
-- of each production should be linked *in the order of execution*.
class HasAnnotation a => ControlFlowNode a where
  -- | connects all the nodes in the subtrees of this tree
  connect   :: (Data b, Typeable b) => a (CFAAnnot b) -> Builder (a (CFAAnnot b))
  label     :: (Data b, Typeable b) => a (CFAAnnot b) -> String

-- | the label for the corresponding graph node
genericLabel :: (Data a) => a -> String
genericLabel = showConstr . toConstr
  
-- instance ControlFlowNode JavaScript where
--   entryNode js = js^.annot.cfgNode
--   exitNodes js@(Script _ body) = listExitNodes (js^.annot.cfgNode) body
--   connect js@(Script _ []) = return js
--   connect (Script a body) =
--     let (body', cfg) = constructCFG body
--     in  return $ Script a body' & annot.bcfg .~ Just cfg

instance ControlFlowNode Expression where
  connect e =
    let exit = e^.exitNode
        entry = e^.entryNode
    in
    case e of
     ArrayLit _ [] -> flow entry exit >> return e
     ArrayLit a elements -> ArrayLit a <$> connectMaybeList elements
                                       <*  flow entry (firstJust elements)
                                       <*  flow (lastJust elements) exit
                                       <*  (when (null (catMaybes elements))
                                            (flow entry exit))
     ObjectLit _ [] -> flow entry exit >> return e
     ObjectLit a props -> ObjectLit a <$> connectList props
                          <* flow entry (firstJust [listEntryNode props
                                                          ,Just exit])
                          <* flow (listExitNode props) exit
     DotRef a obj f -> DotRef a <$> connectWithExceptions obj <*> return f
                                <*  flow entry obj
                                <*  flow obj exit
     BracketRef a obj key ->
       BracketRef a <$> connectWithExceptions obj <*> connectWithExceptions key
                    <* flow entry obj
                    <* flow obj key
                    <* flow key exit
     NewExpr a ctor args -> NewExpr a <$> connectWithExceptions ctor
                                      <*> connectListWithExceptions args
                                      <* flow entry ctor
                                      <* if null args then flow ctor exit
                                         else flow ctor (maybeHead args) <*
                                              flow (maybeLast args) exit
     PrefixExpr a op expr -> PrefixExpr a op <$> connectWithExceptions expr
                                             <* flow entry expr
                                             <* flow expr exit
     UnaryAssignExpr a op lval ->
       (UnaryAssignExpr a op <$> connectWithExceptions lval
                             <*  flow entry lval
                             <*  flow lval exit) >>= connectExceptions
     InfixExpr a op e1 e2 ->
       InfixExpr a op <$> connectWithExceptions e1 <*> connectWithExceptions e2
       <* flow entry e1
       <* case op of
           OpLOr  -> flowLAndOr exit e1 e2
           OpLAnd -> flowLAndOr exit e1 e2
           _      -> flow e1 e2 >> flow e2 exit
     CondExpr a guard ethen eelse -> CondExpr a <$> connectWithExceptions guard
                                                <*> connectWithExceptions ethen
                                                <*> connectWithExceptions eelse
                                                <*  flow entry guard
                                                <*  flow guard ethen
                                                <*  flow guard eelse
                                                <*  flow ethen exit
                                                <*  flow eelse exit
     AssignExpr a OpAssign lval expr ->
       (AssignExpr a OpAssign <$> connect lval <*> connectWithExceptions expr
        <* flow entry lval <* flow lval expr <* flow expr exit)
       >>= connectExceptions
     AssignExpr a op lval expr -> AssignExpr a op <$> connectWithExceptions lval
                                                  <*> connectWithExceptions expr
                                                  <*  flow entry lval
                                                  <*  flow lval expr
                                                  <*  flow expr exit
     CommaExpr _ [] -> flow entry exit >> return e
     CommaExpr a exprs -> CommaExpr a <$> connectListWithExceptions exprs
                                    <*  flow entry (maybeHead exprs)
                                    <*  flow (maybeLast exprs) exit
     CallExpr a func [] -> (CallExpr a <$> connectWithExceptions func
                                       <*> return []
                                       <*  flow entry func
                                       <*  flow func exit) >>= connectExceptions
     CallExpr a func args -> (CallExpr a <$> connectWithExceptions func
                                         <*> connectListWithExceptions args
                                         <*  flow entry func
                                         <*  flow func (maybeHead args)
                                         <*  flow (maybeLast args) exit) >>=
                             connectExceptions
     FuncExpr _ _ _ [] -> flow entry exit >> return e
     FuncExpr a mid params body -> flow entry exit >> return e
       -- this is an intra-procedural graph, so we create a new CFG for the
       -- function body and annotate it with it
       -- let (body', cfg) = constructCFG body
       -- in  return $ (FuncExpr a mid params body') & annot.bcfg .~ Just cfg
     _ -> flow entry exit >> return e
          -- string, regexp, num, bool and null literals,
          -- this and var references don't have internal
          -- connections
  label = genericLabel
  
-- | A common function for connecting logical 'and' and 'or'
-- expressions. Captures the lazy evaluation (of the second argument)
-- in these expressions.
flowLAndOr :: Node -> Expression (CFAAnnot a) -> Expression (CFAAnnot a) -> Builder ()
flowLAndOr exit left right = do flow left right
                                flow left exit
                                flow right exit

instance ControlFlowNode PropAssign where
  connect pa = 
    let entry = pa^.entryNode
        exit  = pa^.exitNode
    in case pa of
      PValue a p e -> PValue a p <$> connect e <* flow entry e
                                               <* flow e exit
      PGet a p ss  -> PGet a p <$> connectList ss
                      <* flow entry (firstJust [listEntryNode ss
                                               ,Just exit])
                      <* flow (listExitNode ss) exit
      PSet a p id ss -> PSet a p id <$> connectList ss 
                        <* flow entry (firstJust [listEntryNode ss
                                                 ,Just exit])
                        <* flow (listExitNode ss) exit
  label = genericLabel
      
instance ControlFlowNode Statement where
  connect s =
    let entry = s^.entryNode
        exit  = s^.exitNode
    in
    case s of
     BlockStmt _ [] -> flow entry exit >> return s
     BlockStmt a body -> BlockStmt a <$> connectList body
                         <* flow entry (maybeHead body)
                         <* flow (maybeLast body) exit
     EmptyStmt _ -> flow entry exit >> return s
     ExprStmt a expr -> ExprStmt a <$> connectWithExceptions expr
                        <* flow entry expr
                        <* flow expr exit
     IfStmt a g then_ else_ ->
       IfStmt a <$> connectWithExceptions g <*> connect then_ <*> connect else_
       <* flow entry g
       <* flow g then_
       <* flow g else_
       <* flow then_ exit
       <* flow else_ exit
     SwitchStmt a guard [] -> SwitchStmt a <$> connectWithExceptions guard
                              <*> return []
                              <*  flow entry guard
                              <*  flow guard exit
     SwitchStmt a guard cases ->
       SwitchStmt a <$> connectWithExceptions guard
       <*> bracketIterSwitch (Switch exit (s^.inlAn.labelSet)) (connectCaseClauses cases)
       <* flow entry guard
       <* flow guard (fromJust $ caseClausesEntry cases)
       <* flow (caseClausesExits cases) exit
     WhileStmt a g body ->
       WhileStmt a <$> connectWithExceptions g <*>
       bracketIterSwitch (Iter exit (g^.entryNode) (s^.inlAn.labelSet))(connect body)
       <* flow entry g
       <* flow g exit
       <* flow g body
       <* flow body g
     DoWhileStmt a body g ->
       DoWhileStmt a <$>
       bracketIterSwitch (Iter exit (g^.entryNode) (s^.inlAn.labelSet)) (connect body)
       <*> connectWithExceptions g
       <* flow entry body
       <* flow body g
       <* flow g exit
       <* flow g body
     BreakStmt _ Nothing ->
       withBreakTarget (connectBreakContinue entry) >> return s
     BreakStmt _ (Just lab) ->
       withLabelledBreakTarget lab (connectBreakContinue entry) >> return s
     ContinueStmt _ Nothing ->
       withContinueTarget (connectBreakContinue entry) >> return s
     ContinueStmt _ (Just lab) ->
       withLabelledContinueTarget lab (connectBreakContinue entry) >> return s
     LabelledStmt a l stmt -> LabelledStmt a l <$> connect stmt
                                               <*  flow entry stmt
                                               <*  flow stmt exit
     ForInStmt a init@(ForInVar _) obj body ->
       ForInStmt a init <$> connectWithExceptions obj <*>
       bracketIterSwitch (Iter exit (obj^.entryNode) (s^.inlAn.labelSet))
       (connect body)
       <* flow entry obj
       <* flow obj exit
       <* flow obj body
       <* flow body obj
     ForInStmt a (ForInVar vd) obj body ->
       ForInStmt a <$> (ForInVar <$> connectWithExceptions vd) <*>
       connectWithExceptions obj <*>
       bracketIterSwitch (Iter exit (vd^.entryNode) (s^.inlAn.labelSet))
       (connect body)
       <* flow entry obj
       <* flow obj exit
       <* flow obj vd
       <* flow vd body
       <* flow body obj
       <* flow body exit
     ForStmt a init mtest minc body ->
       ForStmt a <$> connectForInitWithExceptions init
       <*> (mapM connectWithExceptions mtest)
       <*> (mapM connectWithExceptions minc)
       <*> bracketIterSwitch (Iter exit (head$ catMaybes [(view entryNode)<$>mtest
                                                         ,Just $ body^.entryNode])
                               (s^.inlAn.labelSet)) (connect body)
       -- possible flows originating from entry
       <*  flow entry (firstJust [forInitEntry init
                                 ,(view entryNode) <$> mtest
                                 ,Just $ body^.entryNode])
       -- possible flows originating from the init
       <*  flow (forInitExits init) (firstJust [(view entryNode) <$> mtest
                                               ,Just $ body^.entryNode])
        -- possible flow originating from the test: 1) to the body
       <*  flow (view exitNode <$> mtest) (body^.entryNode)
        -- 2) to the exit (the root)
       <*  flow (view exitNode <$> mtest) exit
        -- possible flow originating from the body
       <*  flow (body^.exitNode) (firstJust [view entryNode <$> minc
                                            ,view entryNode <$> mtest
                                            ,Just $ body^.entryNode])
        -- flows originating from the increment
       <*  flow (view exitNode <$> minc)
           (firstJust [view entryNode <$> mtest, Just $ body^.entryNode])
     TryStmt a body mcatch mfinally ->
       TryStmt a <$> bracketExceptionHandler
                     (head $ catMaybes [view entryNode <$> mcatch
                                       ,mfinally >>= listEntryNode
                                       ,Just exit])
                     (mapM connect body)
                 <*> mapM connect mcatch
                 <*> (sequenceA $ connectList <$> mfinally)
                     -- entry to body
                 <*  flow entry (firstJust [listEntryNode body
                                           ,mfinally >>= listEntryNode
                                           ,Just exit])
                     -- body to finally or to exit
                 <*  flow (listExitNode body)
                    (firstJust [mfinally >>= listEntryNode
                               ,Just exit])
                     -- catch to finally or to exit
                 <*  flow (view exitNode <$> mcatch)
                          (firstJust [mfinally >>= listEntryNode, Just exit])
                     -- optionally, finally to exit        
                 <*  flow (mfinally >>= listExitNode) exit
     ThrowStmt a e -> ThrowStmt a <$> connectWithExceptions e
                      <* flow entry e
                      <* let ex = e^.exitNode
                         in  withEnclosingHandler
                             (maybe (exceptionalReturnFlow ex) (flow ex))
     ReturnStmt a Nothing -> do normalReturnFlow entry
                                return s
     ReturnStmt a (Just exp) ->
       ReturnStmt a <$> Just <$> connectWithExceptions exp
       <* flow entry (exp^.entryNode)
       <* normalReturnFlow (exp^.exitNode)
     WithStmt a obj body ->
       WithStmt a <$> connectWithExceptions obj
                  <*> bracketExceptionHandler exit (connect body)
                  <*  flow entry obj
                  <*  flow obj body
                  <*  flow body exit
     VarDeclStmt _ [] -> flow entry exit >> return s
     VarDeclStmt a vds -> VarDeclStmt a <$> connectListWithExceptions vds
                                        <*  flow entry (maybeHead vds)
                                        <*  flow (maybeLast vds) exit
     FunctionStmt _ _ _ [] -> flow entry exit >> return s
     FunctionStmt a id params body -> flow entry exit >> return s
       -- this is an intra-procedural graph, so we create a new CFG for the
       -- function body and annotate it with it
       -- let (body', cfg) = constructCFG body
       -- in return $ (FunctionStmt a id params body') & annot.bcfg .~ Just cfg

  label = genericLabel

instance ControlFlowNode VarDecl where
  connect vd@(VarDecl a v mexpr) =
    let entry = vd^.entryNode
        exit  = vd^.exitNode
    in case mexpr of
        Nothing -> flow entry exit >> return vd
        Just expr -> VarDecl a v <$> (Just <$> connectWithExceptions expr)
                     <* flow entry expr
                     <* flow expr  exit

  label = genericLabel

  
instance ControlFlowNode CatchClause where
  connect cc@(CatchClause a v body) = CatchClause a v <$> mapM connect body
                                      <* flow (cc^.entryNode)
                                         (firstJust [listEntryNode body
                                                    ,Just $ cc^.exitNode])
                                      <* flow (listExitNode body) (cc^.exitNode)

  label = genericLabel

connectBreakContinue :: Node -> Maybe Node -> Builder ()
connectBreakContinue n Nothing = exceptionalReturnFlow n
connectBreakContinue src (Just dst) = flow src dst

{- connection and entry/exit functions for other AST productions that
   don't fit the ControlFlowNode abstraction nicely -}

forInitEntry :: ForInit (CFAAnnot a) -> Maybe Node
forInitEntry i = case i of
  NoInit -> Nothing
  VarInit [] -> Nothing
  VarInit (v:_) -> Just $ v^.entryNode
  ExprInit expr -> Just $ expr^.entryNode
  
forInitExits :: ForInit (CFAAnnot a) -> Maybe Node
forInitExits i = case i of
  NoInit -> Nothing
  VarInit vds -> view exitNode <$> (maybeLast vds)
  ExprInit expr -> Just $ expr^.exitNode
  
connectForInitWithExceptions :: (Data a, Typeable a)
                             => ForInit (CFAAnnot a)
                             -> Builder (ForInit (CFAAnnot a))
connectForInitWithExceptions init = case init of
  NoInit -> return NoInit
  VarInit vds -> VarInit <$> connectListWithExceptions vds
  ExprInit expr -> ExprInit <$> connectWithExceptions expr
  
-- | No ControlFlowNode instance for CaseClause because they can have
-- more than one entry point; use these four functions instead
caseClauseTestEntry :: CaseClause (CFAAnnot a) -> Maybe Node
caseClauseTestEntry c = case c of
                         CaseClause _ g _  -> Just $ g^.entryNode
                         CaseDefault {} -> Nothing

caseClauseTestExit :: CaseClause (CFAAnnot a) -> Maybe Node
caseClauseTestExit c = case c of
                        CaseClause  {} -> Just $ c^.exitNode
                        CaseDefault {} -> Nothing
                        
caseClauseBodyEntry :: CaseClause (CFAAnnot a) -> Node
caseClauseBodyEntry c = case c of
  CaseClause _ _ body -> fromMaybe (c^.exitNode) $ listEntryNode body
  CaseDefault _ body ->  fromMaybe (c^.exitNode) $ listEntryNode body

caseClauseBodyExit :: CaseClause (CFAAnnot a) -> Node
caseClauseBodyExit c = c^.exitNode
 
caseClausesEntry :: [CaseClause (CFAAnnot a)] -> Maybe Node
caseClausesEntry ccs =
  case ccs of
   []                                      -> Nothing
   (c@CaseClause {} :_)                    -> caseClauseTestEntry c
   [c@CaseDefault {}]                      -> Just $ caseClauseBodyEntry c
   (CaseDefault _ _ : c@CaseClause {} : _) -> caseClauseTestEntry c
   (CaseDefault _ _ : CaseDefault _ _ : _) ->
     error "CFG.Construct.caseClausesEntry: illegal switch structure, more than one\ 
        \ default clause"

caseClausesExits :: [CaseClause (CFAAnnot a)] -> [Node]
caseClausesExits [] = []
caseClausesExits cs = 
  case partitionCases cs of
    Left aList -> catMaybes [maybeLast aList >>= caseClauseTestExit 
                            ,maybeLast aList >>= return . caseClauseBodyExit ]
    Right (aList, cdef, bList) -> (caseClauseBodyExit cdef):
                                  maybeToList (caseClauseBodyExit <$> maybeLast bList)
                                    
connectCaseClauses :: forall a. (Data a, Typeable a)
                   => [CaseClause (CFAAnnot a)]
                   -> Builder [CaseClause (CFAAnnot a)]
connectCaseClauses ccs = 
  let connectCaseList :: [CaseClause (CFAAnnot a)] -> Builder ([CaseClause (CFAAnnot a)], Maybe Node, Maybe Node)
      connectCaseList cc = foldM (\(result, mte, mbe) c -> do
                                     (c', te', be') <- connectCase c mte mbe
                                     return (c':result, Just te', Just be'))
                           ([], Nothing, Nothing) cc
      -- | Since case clauses can have two entry and exit points, we
      -- can't use the 'ControlFlowNode' class for them. Instead, we
      -- connect them as a list, but remembering the previous node's
      -- test exits and body exit node respectively. Not using the
      -- state monad because wrapping a state monad atop of another
      -- state monad (builder) is tricky
      connectCase :: CaseClause (CFAAnnot a) -> Maybe Node -> Maybe Node -> Builder (CaseClause (CFAAnnot a), Node, Node)
      connectCase c@(CaseClause a guard body) mprevTestExit mprevBodyExit =
        do guard' <- connectWithExceptions guard
           body'  <- connectList body
           flow (maybeLast body') (c^.exitNode)
           let cte = caseClauseTestEntry c
           flow mprevTestExit cte
           flow mprevBodyExit (caseClauseBodyEntry c)
           return (CaseClause a guard' body', guard'^.exitNode, c^.exitNode)
      connectDefault c@(CaseDefault a body) =
        CaseDefault a <$> connectList body <* flow (maybeLast body) (c^.exitNode)
  in case partitionCases ccs of
    Left aList -> connectCaseList aList >>= \(aList', _, _) -> return aList'
    Right (aList, defClause, bList) -> do
      (aList', atx, abx) <- connectCaseList aList
      flow abx (caseClauseBodyEntry defClause)
      defClause' <- connectDefault defClause
      (bList', btx, bbx) <- connectCaseList bList
      when (null bList) $ flow atx (caseClauseBodyEntry defClause)
      when (not $ null bList) $
        do flow atx (fromJust $ caseClauseTestEntry $ head bList)
           flow (caseClauseBodyExit defClause)$ caseClauseBodyEntry $ head bList
           flow btx (caseClauseBodyEntry defClause)
      return $ aList' ++ (defClause':bList')

-- | Partitions the case-clause list according to 12.11 of ECMA262:
-- A-list, default and B-list
partitionCases :: [CaseClause a] -> Either [CaseClause a] ([CaseClause a] 
                                                          ,CaseClause a
                                                          ,[CaseClause a])
partitionCases cs = 
  let isCase (CaseClause {}) = True
      isCase _               = False
      isDefault (CaseDefault _ _) = True
      isDefault _                 = False
      (aList, rest) = takeWhile2 isCase cs
      (defClause, bList)     = case takeWhile2 isDefault rest of
        ([], [])  -> (Nothing, [])
        ([c], r2) -> (Just c, r2)
        _   -> error "2 or more default clauses in a switch clause list"
  in if isJust defClause then Right (aList, fromJust defClause, bList)
                         else Left aList
     
listEntryNode :: (HasAnnotation n, ControlFlowNode n)
              => [n (CFAAnnot a)] -> Maybe Node
listEntryNode []    = Nothing
listEntryNode (n:_) = Just $ n^.entryNode

listExitNode :: (HasAnnotation n, ControlFlowNode n)
             => [n (CFAAnnot a)] -> Maybe Node
listExitNode ns = view exitNode <$> maybeLast ns

connectList :: (ControlFlowNode x, Data a, Typeable a)
            => [x (CFAAnnot a)]
            -> Builder [x (CFAAnnot a)]
connectList []       = return []
connectList [x]      = singleton <$> connect x 
connectList (x:y:xs) = do -- connect the exit of 'x' to the entry of 'y'
                          flow (x^.exitNode) (y^.entryNode)
                          -- connect inside 'x'
                          x' <- connect x
                          -- connect the rest of the list
                          rest' <- connectList (y:xs)
                          return (x':rest')

connectMaybeList :: (ControlFlowNode x, Data a, Typeable a)
                 => [Maybe (x (CFAAnnot a))]
                 -> Builder [Maybe (x (CFAAnnot a))]
connectMaybeList = liftM snd . foldM f (Nothing, [])
  where f :: (ControlFlowNode x, Data a, Typeable a)
          => (Maybe (x (CFAAnnot a)), [Maybe (x (CFAAnnot a))])
          -> (Maybe (x (CFAAnnot a)))
          -> Builder (Maybe (x (CFAAnnot a)), [Maybe (x (CFAAnnot a))])
        f (prev, out)  Nothing = return (prev, out ++ [Nothing])
        f (Just prev, out) (Just x) = do x' <- connect x
                                         flow (prev^.exitNode) (x'^.entryNode)
                                         return (Just x, out ++ [Just x'])
        f (Nothing, out) (Just x) = do x' <- connect x
                                       return (Just x, out ++ [Just x'])

{- Handling exceptions -}

-- | If the node can throw an exception, this function connects it to
-- the innermost enclosing exception handler or marks it as an exit
-- node otherwise.
connectExceptions :: (ControlFlowNode x, HasAnnotation x) =>
                     x (CFAAnnot a) -> Builder (x (CFAAnnot a))
connectExceptions x = do when (mayThrow x) $
                           let src = x^.exitNode
                               f Nothing = exceptionalReturnFlow src
                               f (Just h) = flow src h
                           in  withEnclosingHandler f
                         return x

exceptionalReturnFlow :: Edge (a, Node) => a -> Builder ()
exceptionalReturnFlow a = use (flowGraph.exceptionalReturnNode) >>= flow a

normalReturnFlow :: Edge (a, Node) => a -> Builder ()
normalReturnFlow a = use (flowGraph.normalReturnNode) >>= flow a

-- | A predicate that tells for a given AST node whether it may throw
-- an exception
mayThrow :: HasAnnotation x => x (CFAAnnot a) -> Bool
mayThrow x = mayThrowNative || mayThrowInterproc
  where mayThrowNative = not $ Set.null $ x^.inlAn.nativeErrors
        mayThrowInterproc = x^.inlAn.transformAnn.funcMayThrow

withEnclosingHandler :: (Maybe Node -> Builder a) -> Builder a
withEnclosingHandler f = use enclosingHandlers >>= g
  where g [] = f Nothing
        g (x:_) = f (Just x)

-- | This function combines connection inside the tree and connection
-- with an exception handler
connectWithExceptions :: (Data a, Typeable a, ControlFlowNode x, HasAnnotation x)
                      => x (CFAAnnot a) -> Builder (x (CFAAnnot a))
connectWithExceptions x = connect x <* connectExceptions x

-- | Connect inside every element of the list and connect every
-- element to exception handlers
connectListWithExceptions :: (ControlFlowNode x, HasAnnotation x, Data a, Typeable a) =>
                             [x (CFAAnnot a)] -> Builder [x (CFAAnnot a)]
connectListWithExceptions xs = connectList xs <* mapM_ connectExceptions xs

takeWhile2 :: (a -> Bool) -> [a] -> ([a], [a])
takeWhile2 _ [] = ([], [])
takeWhile2 p (x:xs) | p x = first (x:) $ takeWhile2 p xs 
                    | otherwise = ([], x:xs)

firstJust :: [Maybe a] -> Maybe a
firstJust = maybeHead . catMaybes

lastJust :: [Maybe a] -> Maybe a
lastJust = maybeLast . catMaybes
