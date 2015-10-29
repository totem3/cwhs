module Chatwork.V0.ChatworkConfig where

import Control.Monad.State
import Chatwork.V0.Type (Auth)

data ChatworkConfig = ChatworkConfig {
                        base :: String,
                        office :: String,
                        user :: String,
                        pass :: String,
                        auth :: Maybe Auth
                      } deriving (Show)

type Chatwork = StateT ChatworkConfig
