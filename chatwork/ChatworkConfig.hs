module Chatwork.ChatworkConfig where

import Control.Monad.Reader
import Data.List (intercalate)

data ChatworkConfig = ChatworkConfig {
                        base :: String,
                        office :: String,
                        user :: String,
                        pass :: String
                      } deriving (Show)

type Chatwork = ReaderT ChatworkConfig
