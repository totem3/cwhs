module Chatwork where

import Control.Monad.Trans.Reader
import Data.List (intercalate)

data ChatworkConfig = ChatworkConfig {
                        base :: String,
                        office :: String,
                        user :: String,
                        pass :: String
                      }

type Chatwork = ReaderT ChatworkConfig

buildUrl :: [String] -> Chatwork IO String
buildUrl paths = do
  _base <- fmap base ask
  return $ _base ++ (intercalate "/" paths)
