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


{-# LANGUAGE QuasiQuotes, DeriveDataTypeable #-} --, OverloadedStrings #-}

-- | This is the entry point of the inliner program. It processes and
-- inteprets command line arguments and abstracts from the two work
-- modes (proxy and standalone) and two content types (pure JavaScript
-- and HTML with JavaScript in it).

module Main where

import Control.Monad.Trans
import Control.Monad.Trans.Error

import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString

import           Data.Char    (toLower)
import           Data.Default.Class
import           Data.List    (isInfixOf)
import           Data.Maybe   (isJust)
import           Data.Set     (Set)
import qualified Data.Set     as Set
import           Numeric      (readDec)

import Codec.MIME.Type
import Network.HTTP.Base
import Network.HTTP.Encoding
import Network.HTTP.Encoding.Character
import Network.HTTP.Headers
import Network.HTTP.Proxy.Server
import Network.HTTP.Server.Logger (logInfo)
import Network.URI                     (URI)

import System.Console.CmdArgs.Explicit
import System.IO

--import Language.ECMAScript5.PrettyPrint
--import SyntaxHelpers.AnnotatedPrint
import Language.ECMAScript5.Syntax (Program(..), Expression(..), Statement (..))
import Language.ECMAScript5.Syntax.Annotations (reannotate)
import Language.ECMAScript5.Syntax.QuasiQuote
import Language.ECMAScript5.Parser (parseFromFile)
import SyntaxHelpers

import           Inliner
import           Inliner.Monad hiding (environment)
import Inliner.EvalParams
import           RTS hiding (Params (..))
import qualified RTS (Params (..))
import           RTS.Policy         (Policy, PolicyDB)
import qualified RTS.Policy as Policy
import Control.Monad.Identity

import Text.Parsec

import Data.Text (Text)
import qualified Data.Text as Text
import Data.String


-- | The entry point to the inliner program. It can be run in two
-- modes:
-- * stand-alone that allows to inline the monitor into a
--   single JavaScript program/file,
-- * and an HTTP proxy server to deliver seamless inlining of the
--   monitor in web-pages and stand-alone scripts.
-- Usage:
-- 1. Stand-alone, standard input: `jest` [--output <output file>]
-- 2. Stand-alone, file input:
--    `jest --input <input file> [--output <output file>]
-- 3. HTTP proxy server: `jest --proxy [<port number>]`
main :: IO Int
main = do params <- processArgs arguments
          let md = imode params
          case md of
            Help -> print $ helpText [] HelpFormatAll arguments
            Batch {} -> do
              hin <- input2handle $ input md
              hout <- output2handle $ output md
              inlineStream (content md) hin hout params
              hClose hin >> hClose hout
            Proxy      {} ->
              proxy (hostName md) (portNumber md) params
          return 0
  where input2handle Nothing = return stdin
        input2handle (Just fname) = openFile fname ReadMode
        output2handle Nothing = return stdout
        output2handle (Just fname) = openFile fname WriteMode


-- | Program parameters
data Parameters = Parameters {imode       :: InliningMode
                             ,stopfn      :: StopFunction
                             ,policysrc   :: PolicySrc
                             ,environment :: RTS.Environment
                             ,forcePrefix :: Maybe String
                             ,opaqueopt   :: Bool
                             ,rtsExtraPath :: Maybe FilePath
                             }
-- | The source of the policy
data PolicySrc = PolicyFile FilePath
               | PolicyDB   FilePath
               | EmptyPolicy

instance Default PolicySrc where
  def = EmptyPolicy

-- | Inlining modes: proxy and standalone
data InliningMode =
    Proxy {hostName :: String, portNumber :: Integer, logFile :: Maybe String}
  | Batch {input :: Maybe String, output :: Maybe String, content :: HTMLJS}
  | Help

data HTMLJS = HTML
            | JS
            deriving (Eq)

-- | Command line arguments specification
arguments :: Mode Parameters
arguments =
  (modes "jest" defaultBatch
   "JavaScript information flow monitor inliner"
   [(modeEmpty defaultBatch) {modeNames = ["batch"]
                             ,modeHelp = "Batch mode (default)"
                             ,modeArgs = ([], Nothing)
                             ,modeGroupFlags = toGroup batchFlags
                             }
   ,(modeEmpty defaultProxy) {modeNames = ["proxy"]
                             ,modeHelp = "HTTP proxy server mode"
                             ,modeArgs = ([], Nothing)
                             ,modeGroupFlags = toGroup proxyFlags
                             }

   ]) {modeGroupFlags = toGroup $ flagHelpSimple addHelp :batchFlags}
  where batchFlags =
          [flagReq ["input", "in"] addInFile "FILE"
           "Input file path. If the flag is not present, standard input is used"
          ,flagReq ["output","out"] addOutFile "FILE" "Output file path. \
                          \If the flag is not present, standard output is used"
          ,flagNone ["html"] addHTML "Consider input HTML"
          ,flagNone ["js"] addJS "Consider input JavaScript (default)"
          ,flagReq ["policy"] addPolicyFile "FILE" "The policy to apply"
          ,flagReq ["prefix"] addForcedPrefix "valid JS identifier" 
           "Monitor namespace prefix"
          ] ++ preludeCommonFlags
        addHelp _ = Parameters Help def def Standalone Nothing False Nothing
        addInFile v p@(Parameters s@(Batch {}) _ _ _ _ _ _) =
          Right $ p{imode = s{input = Just v}}
        addOutFile v p@(Parameters s@(Batch {}) _ _ _ _ _ _) =
          Right $ p{imode = s{output = Just v}}
        addHTML p@(Parameters s@(Batch {}) _ _ _ _ _ _) =
          addBrowserEnvironment True $ p{imode = s{content = HTML}}
        addJS p@(Parameters s@(Batch {}) _ _ _ _ _ _) =
          p{imode = s{content = JS}}
        defaultBatch =
          Parameters (Batch Nothing Nothing JS) def def Standalone Nothing False Nothing
        defaultProxy = Parameters (Proxy "localhost" 3128 Nothing) def def Browser Nothing False Nothing
        proxyFlags =
          [flagReq ["host"] addHost "HOST" 
           "Host name or IP address the server should listen on. Default: localhost"
          ,flagReq ["port"] addPort "PORT"
           "Port number the server should listen on. Default: 3128."
          ,flagOpt "jest.log" ["log"] addLogFile "FILE" "Log file"
          ] ++
          [flagReq ["policy"] addPolicyFile "FILE"
           "The policy to apply (either --policy or --policydb is required"
          ,flagReq ["policydb"] addPolicyDB "FILE"
           "The policy database file (either --policy or --policydb is required)"
          ] ++ preludeCommonFlags
        addHost h par@(Parameters p@(Proxy {}) _ _ _ _ _ _) =
          Right $ par{imode = p{hostName = h}}
        addPort s par@(Parameters p@(Proxy {}) _ _ _ _ _ _) =
          case readDec s of
            [] -> Left $ "The specified port number is not a number: " ++ s
            (n,_):_ -> Right $ par{imode = p{portNumber = n}}
        addLogFile f par@(Parameters p@(Proxy {}) _ _ _ _ _ _) =
          Right $ par{imode = p{logFile = Just f}}
        preludeCommonFlags =
          [flagReq ["onstop"] addStopFn "{loop,report}" "Monitor stop function"
          ,flagBool ["b", "browser"] addBrowserEnvironment
           "Assume browser execution environment. This flag is on by default for\
           \ proxy mode and HTML content in batch mode"
          ,flagBool ["android", "webview"] addAndroidEnvironment
           "Assume Android WebView execution environment."
          ,flagBool ["oo", "opaqueopt"] addOpaqueOpt "Enable optimizations that sacrifice transparency"
          ,flagReq ["rtsappend"] addRTSExtra "FILE"
           "Append extra code to the RTS initializer (position-dependent)"
          ]
        addStopFn "loop" params =
          Right $ params{stopfn = Loop}
        addStopFn "report" params =
          Right $ params{stopfn = Report}
        addStopFn v _ =
          Left $ "Illegal value for stop function: " ++ v
        addPolicyFile f params =
          Right $ params{policysrc = PolicyFile f}
        addPolicyDB f params =
          Right $ params{policysrc = PolicyDB f}
        addBrowserEnvironment b params = 
          if b then params{environment = RTS.Browser} else params
        addAndroidEnvironment b params =
          if b then params{environment = RTS.Android} else params
        addForcedPrefix p params = Right $ params{forcePrefix = Just p}
        addOpaqueOpt b params = params{opaqueopt = b}
        addRTSExtra f params =
          Right $ params {rtsExtraPath = Just f}

-- | Inlines on an input stream
inlineStream :: HTMLJS -- ^ What's the content type?
             -> Handle -- ^ Input stream handle
             -> Handle -- ^ Output stream handle
             -> Parameters -- ^ Inlining parameters
             -> IO ()
inlineStream content hin hout params = do
  input <- hGetContents hin
  noPolicyDBInParams params
  policy <- loadPolicy (policysrc params) Nothing
  env <- maybeLoadRTSExtra (rtsExtraPath params) (environment params)
  let opt = if opaqueopt params then RTS.Opaque else RTS.Transparent
  let pp=RTS.Params (stopfn params) policy env opt
  output <- handleError $ case content of
    HTML -> inlineHTML pp input Nothing
    JS   -> withParsedSource (inlineSource pp $ forcePrefix params) input
  hPutStr hout output
  where handleError :: Inliner a -> IO a
        handleError inl = runInliner inl >>= \x ->
          case x of
            Left err -> fail $ show err
            Right out -> return out
        noPolicyDB :: Parameters -> IO ()
        noPolicyDB params = case policysrc params of
          PolicyDB _ -> fail "Policy databases are not supported in \
                             \standalone mode!"
          _ -> return ()

maybeLoadRTSExtra :: Maybe FilePath -> RTS.Environment -> IO RTS.Environment
maybeLoadRTSExtra Nothing env = return env
maybeLoadRTSExtra (Just fp) env = parseFromFile fp >>= \extra ->
  return (RTS.Custom env (reannotate (const ()) extra))

-- | Main function for the proxy server
proxy :: String -> Integer -> Parameters -> IO ()
proxy host port params = proxyMain settings
  where settings = def {responseModifier = inlineResponse params
                       ,cache            = def {queryCache = inlineEvalRq}
                       ,portnum          = port
                       ,hostname         = Just host
                       }

adjustContentLength :: Response ByteString -> Response ByteString
adjustContentLength rsp =
  let contentLength = show $ ByteString.length $ getBody rsp
      newhdrs = map recompute $ getHeaders rsp
      recompute hdr = case hdr of
                       Header HdrContentLength value ->
                         Header HdrContentLength contentLength
                       _ -> hdr
  in  rsp `setHeaders` newhdrs

-- | Inlines on an HTTP response
inlineResponse :: Parameters
               -> Request ByteString
               -> Response ByteString
               -> IO (Response ByteString)
inlineResponse params rq rsp = do
  let uri = rqURI rq
  policy <- loadPolicy (policysrc params) (Just uri)
  let opt = if opaqueopt params then RTS.Opaque else RTS.Transparent
  let pp  = RTS.Params (stopfn params) policy  (environment params) opt
  let irsp f = inlineResponseCommon f rq rsp
  logInfo (logger def) 0 $ "Response content-type: " ++ (show $ fullContentType rsp)
  case contentType rsp of
    Just HTML  -> liftM adjustContentLength $ irsp $ \s -> inlineHTML pp s (Just uri)
    -- The following is a fix to a bug where a piece of JS loaded
    -- dynamically and passed to "eval" will get inlined twice: we
    -- just disable inlining on JavaScript responces.
    -- Just JS -> irsp $ inlineSource pp False
    Nothing    -> logInfo (logger def) 0 "Haven't touched the response" >>
                  return rsp
  where contentType :: Response a -> Maybe HTMLJS
        contentType rsp = case (isHTMLContent rsp, isJSContent rsp) of
          (Just True, Just True) -> Nothing -- this shouldn't happen
          (Just True, _        ) -> Just HTML
          (_        , Just True) -> Just JS
          (_        , _        ) -> Nothing
        -- | Returns true if the content type declared in the response is HTML
        isHTMLContent :: Response s -> Maybe Bool
        isHTMLContent rsp =
          fst (getContentTypeAndCharacterEncoding (rspHeaders rsp))>>= \typ->
          return $ case mimeType typ of
            Text        st -> containsHTML st
            Application st -> containsHTML st
        containsHTML st = let lst = Text.toLower st
                          in (fromString "html" `Text.isInfixOf` lst)
        -- | Returns true if the content type declared in the response
        -- is JavaScript
        isJSContent :: Response s -> Maybe Bool
        isJSContent rsp =
          fst (getContentTypeAndCharacterEncoding (rspHeaders rsp)) >>= \typ->
          -- according to the IANA registry of media types
          return $ case mimeType typ of
            Application st -> containsJS st
            Text        st -> containsJS st
        containsJS st = let lst = Text.toLower st
                        in (fromString "ecmascript" `Text.isInfixOf` lst)
                        || (fromString "javascript" `Text.isInfixOf` lst)
        fullContentType :: Response a -> Maybe Type
        fullContentType = fst . getContentTypeAndCharacterEncoding . rspHeaders

-- | Performs inlining on an eval code that the app sends to the
-- proxy. Requests for eval inlining are distinguished from ordinary
-- requests with the custom 'X-Inline' header. The value of the header
-- tells the name of the monitor variable, the request body is the
-- JavaScript to be transformed.
inlineEvalRq :: Request ByteString -> IO (Maybe (Response ByteString))
inlineEvalRq rq = case findHeader (HdrCustom "X-Inline") rq of
  Nothing        -> return Nothing
  Just paramsStr -> case parseEvalParams paramsStr of
    Left err -> return $ Just $ errorResponse rq $ show err
    Right ps -> liftInliner (withParsedSource $ inlineEval (mVarName ps) (disallowVarDecls ps)) rq
                >>= \res ->
      case res of
        Left err   -> return $ Just $ errorResponse rq err
        Right mrq -> return $ Just $ Response (2,0,0) "OK"
                     (filter isContentOrCharacterEncHdr $ getHeaders mrq)
                     (getBody mrq)
          where isContentOrCharacterEncHdr (Header name _) = case name of
                  HdrContentType     -> True
                  HdrContentEncoding -> True
                  _                  -> False

liftInliner :: (HasHeaders (r String), HasHeaders (r ByteString), 
                HasBody r)
            => (String -> Inliner String)
            -> r ByteString
            -> IO (Either String (r ByteString))
liftInliner f r = runErrorT $ convertInlinerError (withDecodedBodyM f r) >>=
                              convertEncodingError

inlineResponseCommon :: (String -> Inliner String)
                     -> Request ByteString
                     -> Response ByteString
                     -> IO (Response ByteString)
inlineResponseCommon f rq rsp =
  liftInliner f rsp >>= \res ->
  case res of
    Left err   -> return $ errorResponse rq err
    Right mrsp -> return mrsp

convertInlinerError :: Inliner a -> ErrorT String IO a
convertInlinerError inl = lift (runInliner inl) >>= \res ->
  case res of
    Left ierr -> throwError $ show ierr
    Right x -> return x

convertEncodingError :: Either EncodingError a -> ErrorT String IO a
convertEncodingError (Left encerr) = throwError $ show encerr
convertEncodingError (Right x) = return x

errorResponse :: Request ByteString -> String -> Response ByteString
errorResponse rq msg =
  let err = "The inlining proxy could not complete the request to transform \
            \content from" ++ show (rqURI rq) ++ ". The error encountered \
            \was: " ++ err
  in  Response {rspCode = (5,0,0)
               ,rspReason = err
               ,rspHeaders = [] -- hope this works
               ,rspBody = ByteString.empty}

nothing2false Nothing  = False
nothing2false (Just b) = b

loadPolicy :: PolicySrc -> Maybe URI -> IO Policy
loadPolicy ps muri =
  let maybeLocatePolicy db muri = muri >>= Policy.policyForURI db
  in case ps of
    PolicyFile file ->
      readFile file >>= \src ->
      case Policy.parse src of
        Left err -> fail $ "Couldn't parse the policy file; reason: " ++ show err
        Right p  -> return p
    PolicyDB   file -> readFile file >>= \src ->
      case Policy.parseDB src of
        Left err -> fail $ "Couldn't parse the policy database file; reason: "
                         ++ show err
        Right db  -> case maybeLocatePolicy db muri of
          Nothing -> fail "The URI of the script is unknown, \
                          \or is not in the database."
          Just  p -> return p
    EmptyPolicy   -> return def

noPolicyDBInParams :: Parameters -> IO ()
noPolicyDBInParams params = case policysrc params of
  PolicyDB _ -> fail "Policy databases are not supported in \
                     \standalone mode!"
  _ -> return ()

separateBy :: (a -> Bool) -> [a] -> [[a]]
separateBy p xs = separateBy2 xs []
  where separateBy2 []     racc = [reverse racc]
        separateBy2 (x:xs) racc | p x = reverse racc:separateBy p xs
        separateBy2 (x:xs) racc       = separateBy2 xs (x:racc)
