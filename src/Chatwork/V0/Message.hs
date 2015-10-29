{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Chatwork.V0.Message where

import Control.Applicative ((<*>))
import Control.Monad (mzero)
import Data.Aeson
import Data.Functor ((<$>))
import Text.ParserCombinators.Parsec
import Text.Parsec.Prim (Stream, ParsecT)

data MessageType = Plain
                 | To {aid :: String}
                 | Reply {aid :: String, replyTo :: String}
                 | Quote {aid :: String, time :: Int} deriving Show

data Message = Message {
                 accountId :: Int,
                 messageId :: Int,
                 body :: String,
                 createTime :: Int,
                 updateTime :: Int
               } deriving (Show, Eq)

instance FromJSON Message where
  parseJSON (Object v) = Message
    <$> v .: "aid"
    <*> v .: "id"
    <*> v .: "msg"
    <*> v .: "tm"
    <*> v .: "utm"
  parseJSON _          = mzero

open :: Text.Parsec.Prim.Stream s m Char => Text.Parsec.Prim.ParsecT s u m Char
open = char '['
close :: Text.Parsec.Prim.Stream s m Char => Text.Parsec.Prim.ParsecT s u m Char
close = char ']'

to :: Text.Parsec.Prim.Stream s m Char => Text.Parsec.Prim.ParsecT s u m MessageType
to = do
  string "To:"
  _to <- many (noneOf " ]")
  return To {aid=_to}

reply :: Text.Parsec.Prim.Stream s m Char => Text.Parsec.Prim.ParsecT s u m MessageType
reply = do
  string "返信"
  spaces
  string "aid="
  _aid <- many (noneOf " ")
  spaces
  string "to="
  _to <- many (noneOf " ]")
  return Reply {aid=_aid, replyTo=_to}

mtype :: Text.Parsec.Prim.Stream s m Char => Text.Parsec.Prim.ParsecT s u m MessageType
mtype = do
  open
  t <- (to <|> reply)
  close
  return t

messageType :: Message -> MessageType
messageType msg = orPlain $ parse mtype "message" (body msg)
  where
    orPlain = either (\_ -> Plain) id
