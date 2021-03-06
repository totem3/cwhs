{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Chatwork.V0.Type where

import Network.HTTP.Conduit (CookieJar)
import Data.Aeson
import Data.Aeson.Types
import Data.Map (Map)
import Control.Applicative ((<*>))
import Data.Functor ((<$>))
import Control.Monad (mzero)
import Chatwork.V0.Message
import GHC.Generics

data Auth = Auth { jar :: CookieJar, myid, accessToken :: String } deriving Show

type RoomId = String
type MessageId = String
type AccountId = String

data Response a = Response {
                    status :: Status,
                    result :: a
                  } deriving Show
instance (FromJSON a) => FromJSON (Response a) where
  parseJSON (Object v) = Response
    <$> v .: "status"
    <*> v .: "result"
  parseJSON _          = mzero

data SendChatData = SendChatData {
                      text :: String,
                      room_id :: String,
                      last_chat_id :: String,
                      read :: String,
                      edit_id :: String
                    } deriving (Show, Generic)

instance ToJSON SendChatData

type ResponseLoadChat = Response LoadChat

data Status = Status {
                success :: Bool
              } deriving Show
instance FromJSON Status where
  parseJSON (Object v) = Status
    <$> v .: "success"
  parseJSON _          = mzero

data LoadChat = LoadChat {
                  chatList :: [Message],
                  description :: String,
                  publicDescription :: String
                } deriving Show
instance FromJSON LoadChat where
  parseJSON (Object v) = LoadChat
    <$> v .: "chat_list"
    <*> v .: "description"
    <*> v .: "public_description"
  parseJSON _          = mzero

data ReadChat = ReadChat {
                  readNum :: Int,
                  mentionNum :: Int
                } deriving Show
instance FromJSON ReadChat where
  parseJSON (Object v) = ReadChat
    <$> v .: "read_num"
    <*> v .: "mention_num"
  parseJSON _          = mzero

data GetUpdate = GetUpdate {
                   announceId :: Int,
                   lastId :: String,
                   updateInfo :: UpdateInfo
                 } deriving Show
data UpdateInfo = UpdateInfo {
                    account :: Account,
                    category :: [String],
                    contact :: Contact,
                    num :: Int,
                    room :: Room
                  } deriving Show

data Room = Empty [Char] | Room {getRoom :: Map String RoomInfo} deriving Show
data Account = EmptyA [Char] | Account {ai :: AccountInfo} deriving Show
data AccountInfo = AccountInfo ( Map String Int ) deriving Show
instance FromJSON Account where
  parseJSON (Array v) = EmptyA <$> parseJSON "[]"
  parseJSON (Object v) = Account <$> v .: "a"
instance FromJSON AccountInfo where
  parseJSON val = AccountInfo <$> parseJSON val

data Contact = EmptyC [Char] | Contact {ci :: Maybe [Int]} deriving Show
instance FromJSON Contact where
  parseJSON (Array v) = EmptyC <$> parseJSON "[]"
  parseJSON (Object v) = Contact <$> v .:? "a"

data RoomInfo = RoomInfo {
              _i  :: Maybe Int,
              _ld :: Maybe Int,
              _p  :: Maybe Int
            } deriving Show
instance FromJSON GetUpdate where
  parseJSON (Object v) = GetUpdate
    <$> v .: "announce_id"
    <*> v .: "last_id"
    <*> v .: "update_info"
  parseJSON _          = mzero
instance FromJSON UpdateInfo where
  parseJSON (Object v) = UpdateInfo
    <$> v .: "account"
    <*> v .: "category"
    <*> v .: "contact"
    <*> v .: "num"
    <*> v .: "room"
  parseJSON _          = mzero
instance FromJSON Room where
  parseJSON (Array v) = Empty <$> parseJSON "[]"
  parseJSON val = Room <$> parseJSON val
instance FromJSON RoomInfo where
  parseJSON (Object v) = RoomInfo
    <$> v .:? "i"
    <*> v .:? "ld"
    <*> v .:? "p"
  parseJSON _          = mzero


data RespGetRoomInfo = RespGetRoomInfo { runRoomInfo :: GetRoomInfo } deriving Show
instance FromJSON RespGetRoomInfo where
  parseJSON (Object v) = RespGetRoomInfo
    <$> v .: "room_dat"

data GetRoomInfo = GetRoomInfo { runRoomData :: Map String RoomData } deriving Show
instance FromJSON GetRoomInfo where
  parseJSON v = GetRoomInfo <$> parseJSON v

data RoomData = RoomData {
                  rdChatNum :: Int,
                  rdChatList :: Maybe [ChatData],
                  rdf :: Int,
                  rdln :: Maybe String,
                  rdlt :: Int,
                  rdChatName :: Maybe String,
                  rdt :: Int,
                  rdtp :: Int
                } deriving Show

instance FromJSON RoomData where
  parseJSON (Object v) = RoomData
    <$> v .: "c"
    <*> v .:? "chat_list"
    <*> v .: "f"
    <*> v .:? "ln"
    <*> v .: "lt"
    <*> v .:? "n"
    <*> v .: "t"
    <*> v .: "tp"

data ChatData = ChatData {
                  _aid :: Int,
                  _id :: Int,
                  _msg :: String,
                  _tm :: Int,
                  _utm :: Int
                } deriving Show
instance FromJSON ChatData where
  parseJSON (Object v) = ChatData
    <$> v .: "aid"
    <*> v .: "id"
    <*> v .: "msg"
    <*> v .: "tm"
    <*> v .: "utm"

data LoadRequest = LoadRequest {
                     infoUpdate :: Map String InfoUpdate,
                     personalUpdate :: [Int],
                     mentionUpdate :: [Int],
                     descUpdate :: [Int],
                     taskUpdate :: TaskUpdate,
                     loadRequestRid :: Int,
                     loadRequestType :: String,
                     loadFileVersion :: Int
                   } deriving Show
instance ToJSON LoadRequest where
  toJSON lr = object [
                "i" .= infoUpdate lr,
                "p" .= personalUpdate lr,
                "m" .= mentionUpdate lr,
                "d" .= descUpdate lr,
                "t" .= emptyArray, -- TODO fix
                "rid" .= loadRequestRid lr,
                "type" .= loadRequestType lr,
                "load_file_version" .= loadFileVersion lr
              ]

data InfoUpdate = InfoUpdate {
                    chutNum :: Int,
                    unreadNum :: Int,
                    lastChatId :: Int,
                    fileNum :: Int,
                    lastFileId :: Int,
                    infoLastUpdate :: Int
                  } deriving Show
instance ToJSON InfoUpdate where
  toJSON iu = object [
                "c"  .= chutNum iu,
                "u"  .= unreadNum iu,
                "l"  .= lastChatId iu,
                "f"  .= fileNum iu,
                "lf" .= lastFileId iu,
                "t"  .= infoLastUpdate iu
              ]

data TaskUpdate = TaskUpdate {
                    taskUpdateRooms :: [TaskUpdateRoom]
                  } deriving Show

data TaskUpdateRoom = TaskUpdateRoom {
                        taskUpdateRoomId :: Int,
                        taskIds :: [Int]
                      } deriving Show

{- 
pdata:
{
  "i": {},
  "p": [
    "17760575"
  ],
  "m": [],
  "d": [],
  "t": {},
  "rid": 0,
  "type": "",
  "load_file_version": "2"
}

{
  "i": {
    "26358354": {
      "c": 20407,
      "u": 4,
      "l": 900646930,
      "f": 0,
      "lf": 0,
      "t": 1447103975
    }
  },
  "p": [],
  "m": [],
  "d": [],
  "t": {},
  "rid": 0,
  "type": "",
  "load_file_version": "2"
}
-}
