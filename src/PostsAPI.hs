{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedLabels #-}
module PostsAPI where

import Prelude hiding (show, index)

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
import qualified Opaleye.Record as R

import qualified Database.Transaction as DB

import Api
import Api.Post as Post

type PostsAPI = APIFor CreatePost (Post R.Hask) PostId

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

postsApi :: DB.Config a -> Server PostsAPI
postsApi c  = serverFor (index c) (show c) (create c) (update c) (destroy c)

runHandler :: DB.Config a -> DB.PG b -> IO b
runHandler = flip $ runReaderT . DB.runTransaction

-- TODO: Write natural transformation to pull out all db effects

index :: DB.Config a -> Response Index [Post R.Hask]
index con = liftIO $ fmap T $ (runHandler con) . R.getAll $ R.query (R.Tab @TestDb @Post)

show :: DB.Config a -> PostId -> Response Api.Show (Post R.Hask)
show con id = do
  post <- liftIO . runHandler con . R.getAll $ findById id
  case post of
    [] -> throwError err404
    (p:_) -> return $ T p

create :: DB.Config a -> CreatePost -> Response Create (Post R.Hask)
create con post = do
  posts <- liftIO $ do
    c_at <- getCurrentTime
    liftIO . runHandler con $ R.insertRet postsTable (Post
      { Post.id = Nothing
      , title = cTitle post
      , body = cBody post
      , created_at = c_at
      , updated_at = c_at
      }) Prelude.id

  case posts of
    [] -> throwError err404
    (x:_) -> return $ T x

update :: DB.Config a -> PostId -> Post R.Hask -> HandlerM NoContent
update = undefined

--update con post = do
--  u_at <- liftIO $ do
--    u_at <- getCurrentTime
--    runUpdateReturning con postsTable (post {updated_at = u_at}) (\p -> updated_at p) :: IO [UTCTime]
--  case u_at of
--    [] -> throwError err404
--    (p:_) -> return () -- $ post {updated_at = u_at }

destroy :: DB.Config a -> PostId -> HandlerM NoContent
destroy con pid = do
  liftIO . runHandler con $ R.delete postsTable (\p-> (Post.id p) .== constant pid)
  return $ NoContent

