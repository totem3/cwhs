module Chatwork where

import Control.Monad.Trans.Reader
import Data.List (intercalate)

data ChatworkConfig = ChatworkConfig {
  apiBase :: String,
  office :: Maybe String,
  user :: String,
  pass :: String
} deriving (Show)

type Chatwork m = ReaderT ChatworkConfig m

class MonadChatwork m where
  getApiBase :: m String

buildUrl :: [String] -> Chatwork IO String
buildUrl paths = do
  base <- fmap apiBase ask
  return $ base ++ (intercalate "/" paths)

{-
*Chatwork> let c = ChatworkConfig {apiBase="https://kcw.kddi.ne.jp/",office=Just "foo",user="hoge",pass="fuga"}
*Chatwork> runReaderT cw c
"https://kcw.kddi.ne.jp/foo/bar"
-} 
