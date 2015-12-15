{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module PostsAPI where

import Prelude hiding (show, index)
import GHC.Generics

import Data.Text hiding (index)
import Data.Time.Clock (UTCTime(..))

import Control.Monad.Trans.Either
import Control.Monad.IO.Class (liftIO)

import Servant hiding (Post)

import Opaleye
import Database.PostgreSQL.Simple as PGS
import Data.Profunctor.Product
import Data.Profunctor.Product.TH

import API

type PostId = Int

data Post' pid t1 t2 ts1 ts2 = Post pid t1 t2 ts1 ts1 deriving (Eq, Show, Generic)

type PostColumn = Post' (Column PGInt4) (Column PGText) (Column PGText) (Column PGTimestamptz) (Column PGTimestamptz)
type Post =  Post' PostId Text Text UTCTime UTCTime

$(makeAdaptorAndInstance "pPost" ''Post')

type PostsAPI = APIFor Post PostId

postsTable :: Table PostColumn PostColumn
postsTable = Table "posts"
                  (pPost $ Post
                    (required "id")
                    (required "title")
                    (required "body")
                    (required "created_at")
                    (required "updated_at")
                  )

postsApi :: Connection -> Server PostsAPI
postsApi c  = serverFor (index c) (show c) (create c) (update c) (destroy c)

index :: Connection -> Response [Post]
index con = liftIO $ runQuery con (queryTable postsTable)

show :: Connection -> PostId -> Response Post
show = undefined

create :: Connection -> Post -> Response ()
create = undefined

update :: Connection -> PostId -> Post -> Response ()
update = undefined

destroy :: Connection -> PostId -> Response ()
destroy = undefined
