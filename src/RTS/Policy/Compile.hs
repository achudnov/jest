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
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

module RTS.Policy.Compile where

import RTS.Policy.Syntax
import Language.ECMAScript5.Syntax
import Language.ECMAScript5.Syntax.Annotations
import Language.ECMAScript5.Syntax.CodeGen
import Language.ECMAScript5.Syntax.QuasiQuote
import Data.Default.Class
import SyntaxHelpers
import RTS.Level
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Arrow
import Data.Bits
import Data.Maybe
import Data.Int
import MyPrelude

{- Note [Compiled policy structure]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(function () {
  function Level() {...}
  Level.bottom = function () {...};
  Level.prototype.join = function (l2) {...};
  Level.prototype.leq = function (l2) {...};
  Level.prototype.toString = function () {...};

  return {levelImpl: Level
         ,locationLevel: function (channel, write) {...}
         };
}
)()
-}

-- | Converts the policy description into a JavaScript object. See
-- Note [Compiled policy structure]
compile :: Default a => Policy -> Expression a
compile (Policy li cm) =
  hygienic (compileLevelImpl li) $
  object [value "levelImpl" $ var internalLevelCtorName
         ,value "locationLevel" $ compileChannelMap li cm]

compileChannelMap :: Default a => 
                     LevelImplementation -> ChannelMap -> Expression a
compileChannelMap li cm = case cm of
  ChannelMapFunction f -> case f of
    FuncExpr _ Nothing _ _ -> reannotate (const def) f
    _ -> error "Policy.Compile.compileChannelMap: incorrect format of \ 
               \a functional representation of a channel map"
  ChannelMapPrincipals cmap -> compilePrincipals cmap li
  ChannelMapTrivial -> lambda [] [returns $ call (var internalLevelCtorName `dot` "bottom") []]

{- Note [Structure of compiled principal locationLevel]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//stringset
function (chan, write) {
  var l = new Level();
  if (/<regex>/.test(chan) && write === false) {
    l.level = {"L1": true, "L2":true};
    return l;
  }
  if (chan === "dom://<id>" && write === true) {
    l.level = {"L3": true};
    return l;
  }
  ...
  if (write) l.level = {"L1": true, ..., "Ln": true}; //Top
  return l;
}

//bitvector
function (chan, write) {
  var l = new Level();

  if (/<regex>/.test(chan) && write === false) {
    l.lev = 3;
    return l;
  }
  if (chan === "dom://<id>" && write === true) {
    l.lev = 4
    return l;
  }
  ...
  if (write) l.level = 2147483647; //Top, 0b0111..1111 (31 1's)
  return l
}
-}

labels = Set.unions . Map.elems


compilePrincipals :: Default a => (Map (Channel, ReadWrite) (Set Label))
                               -> LevelImplementation -> Expression a
compilePrincipals cmap li | (li == StringSet) ||
                            (li == BitVector && Set.size (labels cmap) <= 31) =
  let chanVar = "channel"
      writeVar = "write"
      lid = "l"
      stringSetChanMap =
        Map.map (Set.foldl (\o -> \case NoLabel -> o
                                        Label lab -> Set.insert lab o)
                 Set.empty) cmap
      bitVectorChanMap =
        Map.map (Set.foldl ((.|.)) 0 . Set.map (flip Set.findIndex $ labels cmap)) cmap
      levValue = case li of
                  StringSet -> object . map ((flip value) (bool True)
                                             . propS)
                               . Set.toList . fromJust
                               . flip Map.lookup stringSetChanMap
                  BitVector -> int . int2int32 . fromJust . flip Map.lookup bitVectorChanMap
      topLevel = case li of
                  StringSet -> object $ map ((flip value) (bool True) . propS)
                               $ Set.toList $ Set.unions $ Map.elems stringSetChanMap
                  BitVector -> int 2147483647 -- 0x7FFFFFFF
      defaultChannelCase = [ifte (var writeVar) empty $
                            expr $ ((var lid) `dot` "level") `assign` topLevel
                           ,returns $ var lid
                           ]
     -- (ChannelName, Bool) -> Level
  in lambda [chanVar, writeVar] $ [vardecls [varinit lid newLevel]] ++
     map (\(ch, rw) -> ifte (channelMatcher chanVar ch `land` rwTest writeVar rw) 
                       (block [expr $ var lid `dot` "level" `assign` levValue (ch, rw)
                              ,returns $ var lid]) empty
         )
     (Map.keys cmap) ++ defaultChannelCase

compilePrincipals _ BitVector  =  
  error "Policy.Compile.compilePrincipals: BitVector implementation does not support \
        \more than 32 channels for an automatic origin powerset level \
        \instantiation; please, choose the StringSet implementation for your \
        \policy"
compilePrincipals _  (Custom _) = 
  error "Policy.Compile.compilePrincipals: automatic source powerset level \
        \representation instantiation is not supported for custom level \
        \implementations: please, provide a custom (function) channel mapping \
        \as well"
compilePrincipals _ li = error $ "Failing on" ++ show li

-- | Generates a function expression that takes a string parameter and
-- returns a boolean value saying whether the string parameter matches
-- the channel name
channelMatcher :: Default a => Id a -> Channel -> Expression a
channelMatcher channelParam c = 
  let adaptRegex = concatMap charSubst
      charSubst '*' = "\\*"
      charSubst '|' = "*"
      charSubst c   = [c]
      regexMatcher regex =
        call ((regexp (adaptRegex regex) False True False) `dot` "test") [var channelParam]
      equalityMatcher s = var channelParam `steq` string s
  in case c of
      Network uritpl   -> regexMatcher uritpl
      Cookie domaintpl -> regexMatcher $ "cookie://" ++ domaintpl
      Input (ByElementId eid) -> equalityMatcher $ "dom://" ++ eid
      Function fname mparam ->
        let fchan = "func://" ++ fname
        in  case mparam of
          Nothing -> regexMatcher $ fchan ++ "@|"
          Just pn -> equalityMatcher $ fchan ++ "@" ++ show pn

rwTest :: Default a => Id a -> ReadWrite -> Expression a
rwTest writeParam rw = var writeParam `steq` (bool $ rwBool rw)
  where rwBool Read = False
        rwBool Write = True
                       

newLevel :: Default a => Expression a
newLevel = new (var internalLevelCtorName) []

monitorException :: Default a => String -> Statement a
monitorException msg = throw $ string msg
