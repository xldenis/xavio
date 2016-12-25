{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    ) where

import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import PostsAPI

import Api.Post

import Config

import Database.PostgreSQL.Simple as PGS

type API = "posts" :> PostsAPI

startApp :: Config -> IO ()
startApp conf = run (port conf) (app $ database conf)

app :: PGS.Connection -> Application
app c = serve api $ server c

api :: Proxy API
api = Proxy

server :: PGS.Connection -> Server API
server c = postsApi c
