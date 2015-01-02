{-# LANGUAGE OverloadedStrings #-}
module Chatwork.Type where
import Network.HTTP.Conduit (CookieJar)
import Data.Aeson
import Control.Applicative ((<*>))
import Data.Functor ((<$>))
import Control.Monad (mzero)
import Chatwork.Message

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
                    account :: [String],
                    category :: [String],
                    contact :: [String],
                    num :: Int,
                    room :: [(Int, Room)]
                  } deriving Show
data Room = Room {
              _i  :: Int,
              _ld :: Int,
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
  parseJSON (Object v) = Room
    <$> v .: "i"
    <*> v .: "ld"
    <*> v .:? "p"
  parseJSON _          = mzero

