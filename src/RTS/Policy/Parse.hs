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


{-# LANGUAGE FlexibleContexts, RankNTypes, ImpredicativeTypes #-}

module RTS.Policy.Parse (parse 
                        ,parseDB
                        ,ParseError) where

import Text.Parsec hiding (parse, label)
import qualified Text.Parsec.Prim as Parsec (parse)
import Text.Parsec.Prim hiding (parse, label)
import Text.Parsec.Char
import Text.Parsec.Combinator
import RTS.Policy.Syntax
import Control.Monad.Identity
import qualified Language.ECMAScript5.Parser as JS
import Language.ECMAScript5.ParserState (initialParserState, ParserState)
import Language.ECMAScript5.Syntax
import Language.ECMAScript5.Syntax.Annotations
import Control.Applicative ((<*>), (<*), (<$), (<$>), (*>))
import Data.Char
import Control.Monad (liftM)
import RTS.Level
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Numeric (readDec)

parse :: Stream s Identity Char => s -> Either ParseError Policy
parse = Parsec.parse policy ""

parseDB :: Stream s Identity Char => s -> Either ParseError PolicyDB
parseDB = Parsec.parse database ""

type PolicyParser a = forall s. Stream s Identity Char => Parsec s () a

database :: PolicyParser PolicyDB
database = optional whiteSpace >> many dbentry

dbentry :: PolicyParser (String, Policy)
dbentry = do pat <- symbol pattern
             symbol $ string "=>"
             pol <- policy
             symbol $ char ';'
             return (pat, pol)

-- | A regular expression pattern to match URL's against
pattern :: PolicyParser String
pattern = many (nonEqualsNonBlank <|> equalsNotFollowedByRAngle)
  where nonEqualsNonBlank = satisfy (\c -> not (isSpace c || c == '='))
        equalsNotFollowedByRAngle = do c <- char '='
                                       notFollowedBy $ char '>'
                                       return c
                                            
policy :: PolicyParser Policy
policy = (lexeme $ string "policy") *> (Policy <$> levelImpl <*> channelMap)

levelImpl :: PolicyParser LevelImplementation
levelImpl =
  choice
  [lexeme $ char 'b' >>
   choice [void $ char 'v', void $ string "it" >> optional (string "vector")] >>
   return BitVector
  ,lexeme $ char 's'
   >> choice [void $ char 's', void $ string "tring" >> optional (string "set")]
   >> return StringSet
  ,do lexeme $ string "custom"
      obj <- wrapESParser JS.objectLiteral
      return $ Custom $ removeAnnotations obj
  ]

channelMap :: PolicyParser ChannelMap
channelMap =
  choice [lexeme (char 'p' >> optional (string "rincipals")) >>
          (ChannelMapPrincipals . reversePrincipalMap) <$>
          ((symbol $ char '{') *> 
           (sepEndBy1 labelMap (symbol $ char ';')) <* (symbol $ char '}'))
         ,liftM (ChannelMapFunction . removeAnnotations) $
          (lexeme $ string "function") >> wrapESParser JS.expression
           >>= checkFunc
         ,lexeme (string "trivial") >> return ChannelMapTrivial
         ]
  where checkFunc :: Expression a -> PolicyParser (Expression a)
        checkFunc e@(FuncExpr _ Nothing [_] _) = return e
        checkFunc _ = unexpected "Illegal format for the function channel map"
        reversePrincipalMap :: [(Label, [(Channel, ReadWrite)])] -> (Map (Channel, ReadWrite) (Set Label))
        reversePrincipalMap =
          foldl
          (\m (lab, chans) ->
            foldl (flip $ \chan -> Map.insertWith (Set.union) chan (Set.singleton lab)) m chans
          ) Map.empty

channel :: PolicyParser Channel
channel = choice [channelCookie
                 ,channelInput
                 ,try channelFunctionCall
                 ,channelURITemplate
                 ]

label :: PolicyParser Label
label = symbol $ (NoLabel <$ char '_') <|> (Label <$> many1 (letter <|> digit))

labelMap :: PolicyParser (Label, [(Channel, ReadWrite)])
labelMap = do l <- label
              symbol $ char '='
              cs <- concat <$> sepBy1 directedChannel (symbol $ char ',')
              return (l, cs)

directedChannel :: PolicyParser [(Channel, ReadWrite)]
directedChannel = do mrw <- optionMaybe readWrite
                     chan <- symbol channel
                     return $ case mrw of
                       Just rw -> [(chan, rw)]
                       Nothing -> [(chan, Read), (chan, Write)]

readWrite :: PolicyParser ReadWrite
readWrite = choice [Write <$ string "->", Read <$ string "<-"]

-- | This is a very weak parser as it will only checks for characters
-- that are allowed in a normal uri, but not that the URI is
-- syntactically well-formed according to RFC 3986, with the addition
-- of '|' to represent a wild-card
channelURITemplate :: PolicyParser Channel
channelURITemplate = liftM Network uritemplate

uritemplate :: PolicyParser String
uritemplate = many1 $ choice [urichar, char '|']

urichar :: PolicyParser Char
urichar = choice [asciiletter
                 ,digit
                 ,oneOf "-._~:/?#[]@!$'()*+=%"]
            
-- | This is a very weak parser as it will only checks for characters
-- that are allowed in a normal uri, but not that the URI is
-- syntactically well-formed according to RFC 3986, with the addition
-- of '|' to represent a wild-card                
channelCookie :: PolicyParser Channel
channelCookie = string "cookie://" *> (Cookie <$> uritemplate)

channelInput :: PolicyParser Channel
channelInput = string "dom://" *> (Input <$> ByElementId <$> domid)
        -- DOM identifier (ID, section 6.2, HTML 4.01)
  where domid = do c <- asciiletter
                   cs <- many $ asciiletter <|> digit <|> oneOf "-_:."
                   return $ c:cs

channelFunctionCall :: PolicyParser Channel
channelFunctionCall = do string "func://"
                         fname <- wrapESParser JS.identifier
                         mparam <- optionMaybe (char '@' *> (wrapReadS readDec $ many1 digit))
                         return $ Function (unId fname) mparam

wrapReadS :: ReadS a -> PolicyParser String -> PolicyParser a
wrapReadS r sa = sa >>= \s -> case r s of
  [(a, "")] -> return a
  _         -> fail "Bad parse: could not convert a string to a Haskell value"

asciiletter :: PolicyParser Char
asciiletter = satisfy (\c -> isAscii c && isAlpha c)           
                

whiteSpace :: PolicyParser ()
whiteSpace = void $ skipMany1 space

optspace :: PolicyParser ()
optspace = void $ skipMany space

symbol :: Stream s Identity Char => Parsec s () a -> Parsec s () a
symbol = (<* optspace)

lexeme :: Stream s Identity Char => Parsec s () a -> Parsec s () a
lexeme = (<* whiteSpace)
              
wrapESParser :: forall a s. Stream s Identity Char => Parsec s ParserState a -> Parsec s () a
wrapESParser p = 
  let restoreUserState:: State s u0 -> State s u1 -> State s u0
      restoreUserState st0 st1 = st1{stateUser = stateUser st0}
      mapStateInReply :: (State s u1 -> State s u2) 
                      -> Reply s u1 a -> Reply s u2 a
      mapStateInReply f r = case r of
        Ok a s err -> Ok a (f s) err
        Error err  -> Error err
      wrapState :: State s () -> Identity (Consumed (Identity (Reply s () a)))
      wrapState st = liftM (fmap (fmap (mapStateInReply (restoreUserState st))))
                           (runParsecT p st{stateUser = initialParserState})
  in mkPT wrapState
