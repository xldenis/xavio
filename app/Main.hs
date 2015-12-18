module Main where

import Lib
import Config

import Database.PostgreSQL.Simple as PGS

main :: IO ()
main = do
  conf <- Config 8080 <$> dbCon
  startApp conf

dbCon = PGS.connect PGS.defaultConnectInfo
  { PGS.connectDatabase = "xavio_development" }
