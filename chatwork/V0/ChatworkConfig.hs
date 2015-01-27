module Chatwork.V0.ChatworkConfig where

import Control.Monad.Reader
import Data.List (intercalate)
import Chatwork.V0.Type (Auth)

data ChatworkConfig = ChatworkConfig {
                        base :: String,
                        office :: String,
                        user :: String,
                        pass :: String,
                        auth :: Maybe Auth
                      } deriving (Show)

type Chatwork = ReaderT ChatworkConfig
