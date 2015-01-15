{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Chatwork.Api where

import Network.HTTP.Conduit
import Control.Monad.IO.Class (liftIO)
import Control.Applicative (pure, (<$>))
import System.Environment (getEnv)
import qualified Data.ByteString.Char8 as C
import Text.ParserCombinators.Parsec
import qualified Data.ByteString.Lazy as BL
import Chatwork.Type
import Data.Time.Clock (getCurrentTime)
import System.Locale (defaultTimeLocale)
import Data.Time.Format (formatTime)
import Data.Aeson (decode, FromJSON)
import System.IO.Unsafe (unsafePerformIO)

type QueryParam = [(String, String)]

type ApiResponse a = IO (Maybe (Chatwork.Type.Response a))

apiBase :: String
apiBase = unsafePerformIO (getEnv "CW_API_BASE")

office :: String
office = unsafePerformIO (getEnv "CW_OFFICE")

cmdBase :: String
cmdBase = apiBase ++ "/gateway.php"

loginUrl :: String
loginUrl = apiBase ++ "/login.php?lang=en&args="

loginParams :: [(C.ByteString, String)]
loginParams = [ ("orgkey"     ,  office),
                ("auto_login" ,  "on"),
                ("login"      ,  "Login") ]


getUpdate :: String -> Auth -> ApiResponse GetUpdate
getUpdate _lastId auth = do
  get "get_update" params auth
  where params = [ ("last_id" , _lastId),
                   ("new"     , "1") ]

readChat :: RoomId -> MessageId -> Auth -> ApiResponse ReadChat
readChat roomId lastChatId auth = do
  get "read" params auth
  where params = [ ("last_chat_id" , lastChatId),
                   ("room_id"      , roomId) ]

loadChat :: RoomId -> Auth -> ApiResponse LoadChat
loadChat roomId auth = do
  get "load_chat" params auth
  where params = [ ("room_id"         ,  roomId),
                   ("last_chat_id"    ,  "0"),
                   ("first_chat_id"   ,  "0"),
                   ("jump_to_chat_id" ,  "0"),
                   ("unread_num"      ,  "0"),
                   ("desc"            ,  "1") ]

get :: (FromJSON a) => String -> [(String, String)] -> Auth -> ApiResponse a
get = request "GET"
post :: (FromJSON a) => String -> [(String, String)] -> Auth -> ApiResponse a
post = request "POST"

request :: (FromJSON a) => String -> String -> [(String, String)] -> Auth -> ApiResponse a
request method cmd params auth = do
  time <- formatTime defaultTimeLocale "%s" <$> getCurrentTime
  let query = foldl (join "&") [] $ map zipTuple (params ++ authParams auth ++ [("_", time)])
  let url = cmdBase ++ "?cmd=" ++ cmd ++ "&" ++ query
  _req <- parseUrl url
  let req = _req {cookieJar = Just $ jar auth}
  withManager $ \m -> do
    res <- httpLbs req m
    pure $ decode (responseBody res)
  where
    zipTuple (a, b) = a ++ "=" ++ b
    join s a b = a ++ s ++ b

doHttp method cmd params body auth = do
  time <- formatTime defaultTimeLocale "%s" <$> getCurrentTime
  let query = foldl (join "&") [] $ map zipTuple (params ++ authParams auth ++ [("_", time)])
  let url = cmdBase ++ "?cmd=" ++ cmd ++ "&" ++ query
  _req <- parseUrl url
  let req = (urlEncodedBody body _req){cookieJar = Just $ jar auth}
  withManager $ \m -> do
    res <- httpLbs req m
    pure $ responseBody res
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

login = do
  req <- parseUrl loginUrl
  password <- getEnv "CW_PASSWORD"
  email <- getEnv "CW_USER"
  let _params = fmap pack $ [("email", email), ("password", password)] ++ loginParams
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
    pure auth
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

