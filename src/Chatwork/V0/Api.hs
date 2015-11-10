{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Chatwork.V0.Api (
    module Chatwork.V0.ChatworkConfig
  , module Chatwork.V0.Type
  , module Chatwork.V0.Message
  , login
  , loadChat
  , loadRoom
  , readChat
  , getUpdate
  , sendChat
  , getRoomInfo
)
where

import Network.HTTP.Conduit
import Network.HTTP.Types
import Control.Applicative (pure, (<$>))
import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.ByteString.Char8 as C
import Text.ParserCombinators.Parsec
import qualified Data.ByteString.Lazy as BL
import Chatwork.V0.ChatworkConfig
import Chatwork.V0.Type
import Chatwork.V0.Message
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Aeson (eitherDecode, decode, encode, FromJSON, ToJSON, object, (.=), toJSON)
import System.IO.Unsafe (unsafePerformIO)
import qualified Control.Monad.State as State
import Control.Monad.IO.Class (liftIO)
import Prelude hiding (read)
import qualified Data.Text as T
import Data.List (intercalate)
import qualified Control.Exception as E

type QueryParam = [(String, String)]

type ApiResponse a = Either String (Chatwork.V0.Type.Response a)

sendChat :: String -> String -> Chatwork IO (ApiResponse ())
sendChat rid body = do
  post "send_chat" params postdata
  where
    params = []
    postdata = SendChatData {
                 text = body,
                 room_id = rid,
                 last_chat_id = "0",
                 read = "0",
                 edit_id = "0"
               }

getUpdate :: String -> Chatwork IO (ApiResponse GetUpdate)
getUpdate _lastId = do
  get "get_update" params
  where params = [ ("last_id" , _lastId),
                   ("new"     , "1") ]

readChat :: RoomId -> MessageId -> Chatwork IO (ApiResponse ReadChat)
readChat roomId lastChatId = do
  get "read" params
  where params = [ ("last_chat_id" , lastChatId),
                   ("room_id"      , roomId) ]

loadChat :: RoomId -> Chatwork IO (ApiResponse LoadChat)
loadChat roomId = do
  get "load_chat" params
  where params = [ ("room_id"         ,  roomId),
                   ("last_chat_id"    ,  "0"),
                   ("first_chat_id"   ,  "0"),
                   ("jump_to_chat_id" ,  "0"),
                   ("unread_num"      ,  "0"),
                   ("desc"            ,  "1") ]

loadRoom :: LoadRequest -> Chatwork IO (ApiResponse ())
loadRoom request = do
  post "get_room_info" [] (toJSON request)

getRoomInfo :: RoomId -> Chatwork IO (ApiResponse RespGetRoomInfo)
getRoomInfo roomId = do
  post "get_room_info" [] pdata
    where
      pdata = object [
                "i" .= object [
                  (T.pack roomId) .= object [
                    "c" .= (0 :: Int),
                    "u" .= (0 :: Int),
                    "l" .= (0 :: Int),
                    "t" .= (0 :: Int)
                  ]
                ],
                "p" .= ([] :: [Int]),
                "m" .= ([] :: [Int]),
                "d" .= ([] :: [Int]),
                "t" .= object [],
                "rid" .= (0 :: Int),
                "type" .= ("" :: String),
                "load_file_version" .= ("2" :: String)
              ]

get :: (FromJSON a) => String -> [(String, String)] -> Chatwork IO (ApiResponse a)
get cmd params = request "GET" cmd params Nothing

post :: (FromJSON a, ToJSON b) => String -> [(String, String)] -> b -> Chatwork IO (ApiResponse a)
post cmd params postdata = request "POST" cmd params (Just pdata)
  where
    pdata = RequestBodyLBS $ "pdata=" `BL.append` json
    json = encode postdata

--                         method    command   query paramter        post data
request :: (FromJSON a) => Method -> String -> [(String, String)] -> Maybe RequestBody -> Chatwork IO (ApiResponse a)
request method cmd params postdata = do
  let time = unsafePerformIO $ formatTime defaultTimeLocale "%s" <$> getCurrentTime
  maybeAuth <- fmap auth State.get
  case maybeAuth of
    Just auth -> do
      let query = (fmap.fmap) Just $ fmap (\(a, b) -> (C.pack a, C.pack b)) $ filter (not.null) $ ([("_", time), ("cmd", cmd)] ++ params ++ authParams auth)
      base <- fmap base State.get
      let url = base ++ "gateway.php?"
      __req <- parseUrl url
      let _req = setQueryString query __req
      let contentType = ("Content-Type", C.pack "application/x-www-form-urlencoded; charset=UTF-8")
      let req = case postdata of
                  Just m  -> _req {cookieJar = Just (jar auth), method = method, requestBody = m, requestHeaders = [contentType]}
                  Nothing -> _req {cookieJar = Just (jar auth), method = method}
      manager <- liftIO $ newManager tlsManagerSettings
      runResourceT $ do
        res <- httpLbs req manager
        return $ eitherDecode (responseBody res)
      `catchStateT` httpExceptionHandler
    Nothing -> return (Left "auth failed")

catchStateT :: E.Exception e
            => State.StateT s IO a
            -> (e -> State.StateT s IO a)
            -> State.StateT s IO a
catchStateT a onE = do
    s1 <- State.get
    (result, s2) <- liftIO $ State.runStateT a s1 `E.catch` \e ->
        State.runStateT (onE e) s1
    State.put s2
    return result

httpExceptionHandler :: (FromJSON a) => HttpException -> Chatwork IO (ApiResponse a)
httpExceptionHandler e = do
  liftIO $ print e
  return $ Left (show e)

authParams :: Auth -> QueryParam
authParams auth = [ ("myid"       ,  myid auth),
                    -- ("account_id" ,  myid auth),
                    ("_t"         ,  accessToken auth),
                    ("_v"         ,  "1.80a"),
                    -- ("ver"        ,  "1.80a"),
                    ("_av"        ,  "4") ]

login :: Chatwork IO (Maybe Auth)
login = do
  _base <- fmap base State.get
  req <- parseUrl $ _base ++ "login.php?lang=en&args="
  password <- fmap pass State.get
  email <- fmap user State.get
  office <- fmap office State.get
  let _params = fmap pack $ [("email", email), ("password", password), ("orgkey", office), ("auto_login", "on"), ("login", "Login")]
  let _req = (urlEncodedBody _params req) {method="POST"}
  manager <- liftIO $ newManager tlsManagerSettings
  res <- httpLbs _req manager
  let jar = responseCookieJar res
  let body = responseBody res
  let authinfo = either (\a -> []) id $ fmap (filter (not.emptuple)) (parse parseHTML "" ((C.unpack $ BL.toStrict body) ++ "\n"))
  let myid = lookup "myid" authinfo
  let token = lookup "ACCESS_TOKEN" authinfo
  let auth = case (myid, token) of
               (Just m, Just t) -> Just $ Auth {jar = jar, myid = m, accessToken = t}
               _ -> Nothing
  State.put ChatworkConfig { base = _base, office = office, user = email, pass = password, auth = auth}
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
