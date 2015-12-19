{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}

module PostsAPI where

import Prelude hiding (show, index)
import GHC.Generics

import Data.Text hiding (index)
import Data.Time.Clock (UTCTime(..), getCurrentTime)

import Control.Monad.Trans.Either
import Control.Monad.IO.Class (liftIO)
import Control.Monad
import Control.Arrow (returnA)

import Servant hiding (Post)

import Opaleye
import Database.PostgreSQL.Simple as PGS
import Data.Profunctor.Product
import Data.Profunctor.Product.TH
import Data.Aeson

import API

type PostId = Int

data Post' pid t1 t2 ts1 ts2 = Post
  { postId :: pid
  , title :: t1
  , body  :: t2
  , createdAt :: ts1
  , updatedAt :: ts2
  } deriving (Eq, Show, Generic)

type PostColumn = Post' (Column PGInt4) (Column PGText) (Column PGText) (Column PGTimestamptz) (Column PGTimestamptz)
type WPostColumn = Post' (Maybe (Column PGInt4)) (Column PGText) (Column PGText) (Column PGTimestamptz) (Column PGTimestamptz)
type Post =  Post' PostId Text Text UTCTime UTCTime

$(makeAdaptorAndInstance "pPost" ''Post')

type PostsAPI = APIFor Post PostId

postsTable :: Table WPostColumn PostColumn
postsTable = Table "posts"
                  (pPost $ Post
                    { postId = (optional "id")
                    , title  = (required "title")
                    , body   = (required "body")
                    , createdAt = (required "created_at")
                    , updatedAt = (required "updated_at")
                    }
                  )

instance FromJSON Post where
  parseJSON (Object o)
    = Post <$>
      o .: "id" <*>
      o .: "title" <*>
      o .: "body"  <*>
      o .: "created_at" <*>
      o .: "updated_at"
  parseJSON _ = mzero

instance ToJSON Post where
  toJSON (Post id title body c_at u_at)
     = object [ "id" .= id
             , "title" .= title
             , "body" .= body
             , "created_at" .= c_at
             , "updated_at" .= u_at
             ]
postsApi :: Connection -> Server PostsAPI
postsApi c  = serverFor (index c) (show c) (create c) (update c) (destroy c)

index :: Connection -> Response [Post]
index con = liftIO $ runQuery con (queryTable postsTable)

show :: Connection -> PostId -> Response Post
show con id = do
  post <- liftIO . (runQuery con) . selectPostById $ id
  case post of
    [] -> left err404
    (p:_) -> return p

selectPostById :: PostId -> Opaleye.Query PostColumn
selectPostById id = proc () -> do
  post@(Post pid _ _ _ _) <- (queryTable postsTable) -< ()
  restrict -< pid .== pgInt4 id
  returnA -< post

create :: Connection -> Post -> Response Post
create con post@(Post pid t b c_at u_at) = liftIO $ do
    c_at <- getCurrentTime
    (runInsert con postsTable) $ Post
      { postId = Nothing
      , title  = pgText t
      , body   = pgText b
      , createdAt = pgUTCTime c_at
      , updatedAt = pgUTCTime c_at
      }
    return post

update :: Connection -> PostId -> Post -> Response ()
update = undefined

destroy :: Connection -> PostId -> Response ()
destroy con pid = liftIO $ do
  runDelete con postsTable (\p-> (postId p) .== pgInt4 pid)
  return ()
