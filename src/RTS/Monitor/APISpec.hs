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
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module RTS.Monitor.APISpec where

import Language.ECMAScript5.Parser
import Language.ECMAScript5.ParserState
import Language.ECMAScript5.Syntax
import Language.ECMAScript5.Syntax.Annotations
import Language.ECMAScript5.Syntax.CodeGen
import Language.ECMAScript5.Syntax.QuasiQuote
import Data.Default.Class
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Control.Applicative hiding (Const)
import Data.Char
import Data.Data (Data)
import Data.List
import Data.Typeable (Typeable)
import SyntaxHelpers
import Control.Arrow (first)
import Control.Monad
import System.IO
import Data.Maybe
import MyPrelude
import Data.Generics.Uniplate.Operations
import Data.Set (Set)
import qualified Data.Set as Set
import SyntaxHelpers
import SyntaxHelpers.JSLink
import Control.Monad.Reader.Class
import qualified Data.Map as Map
import Language.Haskell.TH.Syntax

loadAndParse :: FilePath -> Q (Program ParserAnnotation)
loadAndParse f = addDependentFile f >> runIO (parseFromFile f)

lowerFirst :: String -> String
lowerFirst [] = []
lowerFirst (c:cs) = toLower c:cs

class MonitorPrefix a where
  getPrefix :: a -> String

instance MonitorPrefix String where
  getPrefix = id

mkBindings :: Program a -> String -> Q [Dec]
mkBindings js initName =
  let md d@(name, _) = liftM2 (++) (mkDecl False d (lowerFirst name))
                       (mkDecl True d ((lowerFirst name) ++ "M"))
      decls = getTopDecls js
  in  liftM concat $ sequence [concatMapM md $ Map.toList decls
                              ,declInfixBindings
                              ,declInfixRefs
                              ,declPrefixBindings
                              --,declUnaryAssignBindings
                              ,mkInit js initName
                              ]

mkInit :: Program a -> String -> Q [Dec]
mkInit js bindTo =
  let name = mkName bindTo
      js2 = removeAnnotations js
  in  sequence [liftM (SigD name) [t|Default a => Program a|]
               ,liftM (\body -> FunD name [Clause [] (NormalB body) []])
                [|redef js2|]
               ]


mkDecl :: Bool -> (String, DeclKind) -> String -> Q [Dec]
mkDecl autolink (name, decl) bindTo =
  let mvnv = mkName "monitor"
      bind = mkName bindTo
  in case decl of
      Function  params ->
        let pars = map mkName params in
        do bodyE <- if autolink then mkFnCallRenaming name params
                    else mkFnCall name params
           sequence [liftM (SigD $ bind) $ bindingType autolink $ length params
                    ,return $ FunD bind
                     [Clause (map VarP pars) (NormalB bodyE) []]
                    ]
      Var ->
        let getName = mkName $ "get" ++ bindTo
            setName = mkName $ "set" ++ bindTo
        in do frE <- if autolink then mkBindingRefRenaming name
                     else mkBindingRef name
              body <- if autolink then [|\val-> $(return frE) >>= \f-> return (AssignExpr def OpAssign f val)|]
                      else [|\val-> AssignExpr def OpAssign ($(return frE)) val|]
              sequence [liftM (SigD getName) $ bindingType autolink 0
                       ,return $ FunD getName [Clause [] (NormalB frE) []]
                       ,liftM (SigD setName) $ bindingType autolink 1
                       ,return $ FunD setName [Clause [] (NormalB body) []]
                       ]
      Const -> 
         do frE <- if autolink then mkBindingRefRenaming name
                               else mkBindingRef  name
            sequence [liftM (SigD bind) $ bindingType autolink 0
                     ,return $ FunD bind [Clause [] (NormalB frE) []]
                     ]
                  
mkFnCall ::String -> [String] -> Q Exp
mkFnCall fname params =
  let pars = return $ ListE $ map (VarE . mkName) params in
  [|call $(mkBindingRef fname) $(pars)|]

mkFnCallRenaming :: String -> [String] -> Q Exp
mkFnCallRenaming fname params =
     [|call <$> $(mkBindingRefRenaming fname) <*> return $(return $ ListE (map (VarE . mkName) params))|]

-- | Makes a direct binding reference, without renaming/linking.
mkBindingRef :: String -> Q Exp
mkBindingRef name = [|var $ ident name|]

-- | Makes a field reference with renaming, assuming we are in a reader monad that can
-- supply the monitor prefix. Note: functions 'String -> Expression a'
-- are also an instance of the monad.
mkBindingRefRenaming :: String -> Q Exp
mkBindingRefRenaming name =
  [|asks getPrefix >>= \prefix -> return $ var $ ident $ prefix ++ name|]

bindingType :: Bool -> Int -> Q Type
bindingType autolink arity =
  if autolink then [t|forall a m p. (Default a, MonitorPrefix p, MonadReader p m, Applicative m) => $(bindingType2 autolink arity)|]
  else [t|forall a. Default a => $(bindingType2 autolink arity)|]

bindingType2 autolink arity = 
  let exptyp = (\a -> [t|Expression $a|]) (return $ VarT $ mkName "a")
  in if arity > 0 then [t|$(exptyp) -> $(bindingType2 autolink (arity-1))|]
     else if autolink then (\m -> [t|$m $(exptyp)|]) (return $ VarT $ mkName "m")
          else [t|$(exptyp)|] 


mkOpBindings :: Name -> Bool -> String -> Int -> (String -> String) -> [Name] -> Q [Dec]
mkOpBindings tyName autolink dispatchName arity conNameTrans exceptions =
  let mkCase :: Con -> Match
      mkCase c = Match (con2pat c)
                 (NormalB $ AppE (VarE $ mkName $ conNameTrans $ nameBase $ conName c)
                                 (VarE mvn)) []
      mkCaseM :: Con -> Match
      mkCaseM c = Match (con2pat c) (NormalB $ VarE $ mkName $ conNameTrans $ nameBase $ conName c) []
      name = mkName dispatchName
      mvn = mkName "mv"
      op = mkName "op"
      shouldProcess :: Con -> Bool
      shouldProcess c = conName c `notElem` exceptions
  in
  do (TyConI (DataD _ _ _ cons _)) <- reify tyName
     let body = NormalB $ CaseE (VarE op) $
                map (if autolink then mkCaseM else mkCase) $ filter shouldProcess cons
     let params = if autolink then [op] else [op, mvn]
     btype <- if autolink then [t|forall a m p. (Default a, MonitorPrefix p, MonadReader p m, Applicative m) => $(return $ ConT tyName) -> $(bindingType2 True arity)|]
              else [t|forall a. Default a => String -> $(return $ ConT tyName) -> $(bindingType2 False arity)|]
     return [SigD name btype
            ,FunD name [Clause (map (VarP) params) body []]
            ]

-- | Makes bindings to references to methods implementing the operators
mkOpRefs :: Name -> String -> (String -> String) -> Q [Dec]
mkOpRefs tyName dispatchName conNameTrans =
  let mkCase :: Con -> Q Match
      mkCase c = do fr <- mkBindingRefRenaming $ conNameTrans $ nameBase $ conName c
                    return $ Match (con2pat c) (NormalB fr) []
  in do (TyConI (DataD _ _ _ cons _)) <- reify tyName
        cases <- mapM mkCase cons
        let body = NormalB $ CaseE (VarE $ mkName "op") cases
        let name = mkName dispatchName
        sequence
          [liftM (SigD name)
           [t|forall a m p. (Default a, MonitorPrefix p, MonadReader p m, Applicative m) => $(return $ ConT tyName) -> m (Expression a)|]
          ,return $ FunD name [Clause [VarP $ mkName "op"] body []]
          ]
  
con2pat :: Con -> Pat
con2pat con = case con of
  NormalC name tys -> ConP name (replicate (length tys) WildP)
  RecC name tys -> ConP name (replicate (length tys) WildP)
  InfixC _ name _ -> ConP name [WildP, WildP]
  ForallC _ _ con2 -> con2pat con2

conName :: Con -> Name
conName con = case con of
  NormalC name _ -> name
  RecC name _ -> name
  InfixC _ name _ -> name
  ForallC _ _ con2 -> conName con2

-- Generate a correspondence from Ops to their implementations in the
-- monitor
declInfixBindings :: Q [Dec]
declInfixBindings = mkOpBindings ''InfixOp True "infixOpM" 2 infixConNameTrans
                    ['OpLOr, 'OpLAnd]

infixConNameTrans = (++ "M") .map toLower

declInfixRefs :: Q [Dec]
declInfixRefs = mkOpRefs ''InfixOp "infixOpRefM" infixRefConNameTrans

infixRefConNameTrans = map toLower

-- Generate a correspondence from Ops to their implementations in the
-- monitor
declPrefixBindings :: Q [Dec]
declPrefixBindings = mkOpBindings ''PrefixOp True "prefixOpM" 1 prefixConNameTrans ['PrefixDelete, 'PrefixVoid]

prefixConNameTrans = (++ "M") . ("op" ++) . (map toLower) . drop 6

-- declUnaryAssignBindings :: Q [Dec]
-- declUnaryAssignBindings = mkOpBindings ''UnaryAssignOp True "unaryAssignOpM" 1 unaryAssignConNameTrans []

-- unaryAssignConNameTrans = (++ "M") . map toLower
