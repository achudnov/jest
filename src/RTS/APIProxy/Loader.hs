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


{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module RTS.APIProxy.Loader (loadProxies) where

import Language.ECMAScript5.Syntax
import Language.ECMAScript5.Syntax.CodeGen
import SyntaxHelpers
import SyntaxHelpers.JSFunc
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Control.Monad.Except hiding (lift)
import Data.Data (Data)
import Data.Typeable (Typeable)
import Data.Generics.Uniplate.Data
import MyPrelude
import Data.Default.Class
import Data.Default.Instances.Base

data APIError = NoGlobalObject
              | InvalidSpecFormat
              deriving (Show)

proxyLibPath = "rts/proxy.js"

loadProxies :: [String] -> Q Exp
loadProxies files =
  do proxyInit <- liftM catScripts $ mapM loadAndParse files
     proxyLib <- loadAndParse proxyLibPath
     case runExcept $ prepareProxies proxyInit of
      Left err -> fail $ show err
      Right (JSFunc ps body, bindings) ->
        lift $ (JSFunc ps $ (unProgram $ redef proxyLib) ++ body ++
                [returns $ var "global"]
               ,bindings)
     
catScripts = Program def . concatMap unProgram

prepareProxies :: (Data a, Typeable a) => Program a -- proxy descriptions
               -> Except APIError
               (JSFunc ()
                -- ^ A function that initialises all the proxies and
                -- returns a reference to the global object
                -- proxy
               ,[String] -- ^ All the global object bindings
               )
prepareProxies js = if (hasGlobalObject js) then
                      do bindings <- globalBindings js
                         return (JSFunc [] $ unProgram $ redef js, bindings)
                    else throwError NoGlobalObject

globalBindings :: forall a. (Data a, Typeable a) => Program a -> Except APIError [String]
globalBindings js =
  let fRefs = [f | (CallExpr _ (VarRef _ (Id _ "addFields"))
                    [VarRef _ (Id (_::a) "global"), f]) <- universeBi js]
      collectFields :: Expression a -> Except APIError [String]
      collectFields e =
        case e of
         VarRef _ v ->
           case [e | (VarDecl (_::a) v2 (Just e)) <- universeBi js, unId v == unId v2] of
            [ObjectLit _ fs] -> mapM getFieldName fs
            _ -> throwError InvalidSpecFormat
         ObjectLit _ fs -> mapM getFieldName fs
         _ -> throwError InvalidSpecFormat
      getFieldName :: PropAssign a -> Except APIError String
      getFieldName pa = getPropName $ case pa of
        PValue _ p _   -> p
        PGet   _ p _   -> p
        PSet   _ p _ _ -> p
      getPropName :: Prop a -> Except APIError String
      getPropName p = case p of
        PropId _ f     -> return f
        PropString _ f -> return f
        PropNum {}     -> throwError InvalidSpecFormat
  in  concatMapM collectFields fRefs

hasGlobalObject :: forall a. (Data a, Typeable a) => Program a -> Bool
hasGlobalObject js = length [()| (VarDecl _ (Id (_::a) "global") _) <- universeBi js] == 1
