{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Chatwork.V0.Api (
    module Chatwork.V0.ChatworkConfig
  , module Chatwork.V0.Type
  , module Chatwork.V0.Message
  , login
  , loadChat
  , readChat
  , getUpdate
  , sendChat
)
where

import Network.HTTP.Conduit
import Network.HTTP.Types
import Control.Applicative (pure, (<$>))
import qualified Data.ByteString.Char8 as C
import Text.ParserCombinators.Parsec
import qualified Data.ByteString.Lazy as BL
import Chatwork.V0.ChatworkConfig
import Chatwork.V0.Type
import Chatwork.V0.Message
import Data.Time.Clock (getCurrentTime)
import System.Locale (defaultTimeLocale)
import Data.Time.Format (formatTime)
import Data.Aeson (decode, encode, FromJSON, ToJSON)
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.Reader (ask)
import Control.Monad.IO.Class (liftIO)
import Prelude hiding (read)

type QueryParam = [(String, String)]

type ApiResponse a = Maybe (Chatwork.V0.Type.Response a)

sendChat :: String -> String -> Auth -> Chatwork IO (ApiResponse ())
sendChat rid body auth = do
  post "send_chat" params postdata auth
    where
      params = []
      postdata = SendChatData {
                   text = body,
                   room_id = rid,
                   last_chat_id = "0",
                   read = "1",
                   edit_id = "0"
                 }

getUpdate :: String -> Auth -> Chatwork IO (ApiResponse GetUpdate)
getUpdate _lastId auth = do
  get "get_update" params auth
  where params = [ ("last_id" , _lastId),
                   ("new"     , "1") ]

readChat :: RoomId -> MessageId -> Auth -> Chatwork IO (ApiResponse ReadChat)
readChat roomId lastChatId auth = do
  get "read" params auth
  where params = [ ("last_chat_id" , lastChatId),
                   ("room_id"      , roomId) ]

loadChat :: RoomId -> Auth -> Chatwork IO (ApiResponse LoadChat)
loadChat roomId auth = do
  get "load_chat" params auth
  where params = [ ("room_id"         ,  roomId),
                   ("last_chat_id"    ,  "0"),
                   ("first_chat_id"   ,  "0"),
                   ("jump_to_chat_id" ,  "0"),
                   ("unread_num"      ,  "0"),
                   ("desc"            ,  "1") ]

get :: (FromJSON a) => String -> [(String, String)] -> Auth -> Chatwork IO (Maybe (Chatwork.V0.Type.Response a))
get cmd params auth = request "GET" cmd params Nothing auth

post :: (FromJSON a, ToJSON b) => String -> [(String, String)] -> b -> Auth -> Chatwork IO (Maybe (Chatwork.V0.Type.Response a))
post cmd params postdata auth = request "POST" cmd params (Just pdata) auth
  where
    pdata = RequestBodyLBS $ "pdata=" `BL.append` json
    json = encode postdata

--                         method    command   query paramter        post data
request :: (FromJSON a) => Method -> String -> [(String, String)] -> Maybe RequestBody -> Auth -> Chatwork IO (Maybe (Chatwork.V0.Type.Response a))
request method cmd params postdata auth = do
  let time = unsafePerformIO $ formatTime defaultTimeLocale "%s" <$> getCurrentTime
  let query = foldl (join "&") [] $ map zipTuple (params ++ authParams auth ++ [("_", time)])
  base <- fmap base ask
  let url = base ++ "gateway.php?cmd=" ++ cmd ++ "&" ++ query
  _req <- parseUrl url
  let req = case postdata of
              Just m  -> _req {cookieJar = Just (jar auth), method = method, requestBody = m}
              Nothing -> _req {cookieJar = Just (jar auth), method = method}
  withManager $ \m -> do
    res <- httpLbs req m
    pure $ decode (responseBody res)
  where
    zipTuple (a, b) = a ++ "=" ++ b
    join s a b = a ++ s ++ b

authParams :: Auth -> QueryParam
authParams auth = [ ("myid"       ,  myid auth),
                    ("account_id" ,  myid auth),
                    ("_t"         ,  accessToken auth),
                    ("_v"         ,  "1.80a"),
                    ("ver"        ,  "1.80a"),
                    ("_av"        ,  "4") ]

login :: Chatwork IO (Maybe Auth)
login = do
  _base <- fmap base ask
  req <- parseUrl $ _base ++ "login.php?lang=en&args="
  password <- fmap pass ask
  email <- fmap user ask
  office <- fmap office ask
  let _params = fmap pack $ [("email", email), ("password", password), ("orgkey", office), ("auto_login", "on"), ("login", "Login")]
  let _req = (urlEncodedBody _params req) {method="POST"}
  withManager $ \m -> do
    res <- httpLbs _req m
    let jar = responseCookieJar res
    let body = responseBody res
    let authinfo = either (\a -> []) id $ fmap (filter (not.emptuple)) (parse parseHTML "" ((C.unpack $ BL.toStrict body) ++ "\n"))
    let myid = lookup "myid" authinfo
    let token = lookup "ACCESS_TOKEN" authinfo
    let auth = case (myid, token) of
                 (Just m, Just t) -> Just $ Auth {jar = jar, myid = m, accessToken = t}
                 _ -> Nothing
    liftIO.pure $ auth
  where
    pack (a, b) = (a, C.pack b)
    emptuple (a, b) = a == "" && b == ""
    find key ts = head $ map snd $ filter (\t -> fst t == key) ts

parseHTML :: GenParser Char st [(String, String)]
parseHTML = do
  result <- many line
  eof
  return result

line :: GenParser Char st (String, String)
line =
    do result <- anyAuthInfo
       optional (char ';')
       optional (char '\r')
       eol
       return result

jsString :: GenParser Char st String
jsString = do
  oneOf  "\"'"
  s <- many (noneOf "'\"")
  oneOf  "\"'"
  return s

anyAuthInfo :: GenParser Char st (String, String)
anyAuthInfo = do
  var
  try authInfo <|> do
    many (noneOf "\n")
    anyAuthInfo
  <|> (many (noneOf "\n") >> return ("",""))

authInfo = _myid <|> _token

_myid = jsStrVar "myid"
_token = jsStrVar "ACCESS_TOKEN"

var :: GenParser Char st String
var = string "var" >> spaces >> return []

jsStrVar :: String -> GenParser Char st (String, String)
jsStrVar str = do
  string str
  spaces
  char '='
  spaces
  s <- jsString
  return (str, s)

eol :: GenParser Char st Char
eol = char '\n'

