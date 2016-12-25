{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedLabels #-}
module PostsAPI where

import Prelude hiding (show, index)

import Database.PostgreSQL.Simple as PGS
import Database

import Data.Aeson
import Data.Text hiding (index)
import Data.Time.Clock (UTCTime(..), getCurrentTime)

import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Control.Monad
import Control.Monad.Reader

import Control.Arrow (returnA)

import Servant hiding (Post)

import Opaleye

import Api
import Api.Post as Post

type PostsAPI = APIFor CreatePost (Post Hask) PostId

data CreatePost = CreatePost
  { cTitle :: Text
  , cBody :: Text
  }

instance FromJSON CreatePost where
  parseJSON (Object o)
    = CreatePost <$>
      o .: "title" <*>
      o .: "body"
  parseJSON _ = mzero

postsApi :: PGS.Connection -> Server PostsAPI
postsApi c  = serverFor (index c) (show c) (create c) (update c) (destroy c)

-- runHandler :: PGS.Connection -> DB.PG b -> IO b
-- runHandler = flip $ runReaderT . DB.runTransaction

-- TODO: Write natural transformation to pull out all db effects

index :: PGS.Connection -> Response Index [Post Hask]
index con = liftIO $ fmap T $ runQuery con (queryTable postsTable)

show :: PGS.Connection -> PostId -> Response Api.Show (Post Hask)
show con id = do
  post <- liftIO . runQuery con $ findById id
  case post of
    [] -> throwError err404
    (p:_) -> return $ T p

create :: PGS.Connection -> CreatePost -> Response Create (Post Hask)
create con post = do
  posts <- liftIO $ do
    c_at <- getCurrentTime
    liftIO $ runInsertReturning con postsTable (Post
      { Post.id = Nothing
      , title = pgText $ cTitle post
      , body = pgText $ cBody post
      , created_at = pgUTCTime c_at
      , updated_at = pgUTCTime c_at
      }) Prelude.id

  case posts of
    [] -> throwError err404
    (x:_) -> return $ T x

update :: PGS.Connection -> PostId -> Post Hask -> HandlerM NoContent
update = undefined

--update con post = do
--  u_at <- liftIO $ do
--    u_at <- getCurrentTime
--    runUpdateReturning con postsTable (post {updated_at = u_at}) (\p -> updated_at p) :: IO [UTCTime]
--  case u_at of
--    [] -> throwError err404
--    (p:_) -> return () -- $ post {updated_at = u_at }

destroy :: PGS.Connection -> PostId -> HandlerM NoContent
destroy con pid = do
  liftIO $ runDelete con postsTable (\p-> (Post.id p) .== constant pid)
  return $ NoContent

