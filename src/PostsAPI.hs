{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module PostsAPI where

import Prelude hiding (show, index)
import GHC.Generics

import Data.Text hiding (index)
import Data.Time.Clock (UTCTime(..), getCurrentTime)

import Control.Monad.Except
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

selectPostById :: PostId -> Opaleye.Query PostColumn
selectPostById id = proc () -> do
  post@(Post pid _ _ _ _) <- (queryTable postsTable) -< ()
  restrict -< pid .== pgInt4 id
  returnA -< post

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


index :: Connection -> Response Index [Post]
index con = T <$> (liftIO $ runQuery con (queryTable postsTable) )

show :: Connection -> PostId -> Response API.Show Post
show con id = do
  post <- liftIO . (runQuery con) . selectPostById $ id
  case post of
    [] -> throwError err404
    (p:_) -> return $ T p

create :: Connection -> Post -> Response Create Post
create con post@(Post pid t b c_at u_at) = do
  pid <- liftIO $ do
    c_at <- getCurrentTime
    (runInsertReturning con postsTable (Post
      { postId = Nothing
      , title  = pgText t
      , body   = pgText b
      , createdAt = pgUTCTime c_at
      , updatedAt = pgUTCTime c_at
      })
      (\p -> postId p)) :: IO [PostId]
  case pid of
    [] -> throwError err404
    (x:_) -> return $ T (post {postId = x})

update :: Connection -> PostId -> Post -> HandlerM NoContent
update = undefined

--update con post = do
--  u_at <- liftIO $ do
--    u_at <- getCurrentTime
--    runUpdateReturning con postsTable (post {updatedAt = u_at}) (\p -> updatedAt p) :: IO [UTCTime]
--  case u_at of
--    [] -> throwError err404
--    (p:_) -> return () -- $ post {updatedAt = u_at }

destroy :: Connection -> PostId -> HandlerM NoContent
destroy con pid = liftIO $ do
  runDelete con postsTable (\p-> (postId p) .== pgInt4 pid)
  return $ NoContent
