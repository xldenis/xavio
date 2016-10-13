{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    ) where

import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import qualified Database.Transaction as DB
import PostsAPI

import Api.Post

import Config

type API = "posts" :> PostsAPI

startApp :: Config -> IO ()
startApp conf = run (port conf) (app $ database conf)

app :: DB.Config a -> Application
app c = serve api $ server c

api :: Proxy API
api = Proxy

server :: DB.Config a -> Server API
server c = postsApi c
