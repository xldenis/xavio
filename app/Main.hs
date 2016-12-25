module Main where

import Lib
import Config

import Api.Post
import Data.Proxy
import Data.Text as T

import Database.PostgreSQL.Simple as PGS
import qualified Database.Transaction as DB
import Database.Migration

main :: IO ()
main = do
  conf <- Config 8080 <$> config
  startApp conf

config :: IO (PGS.Connection)
config = connect $ PGS.defaultConnectInfo
  { PGS.connectDatabase = "xavio_development" }
