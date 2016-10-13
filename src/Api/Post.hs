{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, DeriveGeneric, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, OverloadedLabels, Arrows, TypeApplications, UndecidableInstances, FlexibleContexts #-}
module Api.Post where
import Prelude hiding (id)

import Data.Aeson
import Data.Profunctor
import Data.Profunctor.Product.Default hiding (def)
import qualified Data.Profunctor.Product.Default as P (def)

import Database.Migration hiding (Column, Col)
import Database.Transaction

import Opaleye hiding (Table(..))

import Opaleye.Record

import Data.Text as T
import Data.Time.Clock

import GHC.Generics

import Control.Arrow
import Control.Monad.Reader

import Web.HttpApiData

newtype PostId = PostId Int
  deriving (Generic, FromJSON, ToJSON, FromHttpApiData)

data Post f = Post
  { id :: Col f "id" PostId
  , title :: Col f "title" Text
  , body  :: Col f "body" Text
  , created_at :: Col f "created_at" UTCTime
  , updated_at :: Col f "updated_at" UTCTime
  } deriving (Generic)

instance ( Profunctor p
         , Applicative (p (Post f))
         , Default p (Col f "id" PostId) (Col g "id" PostId)
         , Default p (Col f "title" Text) (Col g "title" Text)
         , Default p (Col f "body" Text) (Col g "body" Text)
         , Default p (Col f "created_at" UTCTime) (Col g "created_at" UTCTime)
         , Default p (Col f "updated_at" UTCTime) (Col g "updated_at" UTCTime)
         ) => Default p (Post f) (Post g) where
  def = Post <$> lmap id P.def
             <*> lmap title P.def
             <*> lmap body P.def
             <*> lmap created_at P.def
             <*> lmap updated_at P.def

instance FromJSON (Post Hask) where
  parseJSON (Object o)
    = Post <$>
      o .: "id" <*>
      o .: "title" <*>
      o .: "body"  <*>
      o .: "created_at" <*>
      o .: "updated_at"
  parseJSON _ = mzero

instance ToJSON (Post Hask) where
  toJSON (Post id title body c_at u_at)
     = object [ "id" .= id
             , "title" .= title
             , "body" .= body
             , "created_at" .= c_at
             , "updated_at" .= u_at
             ]

data TestDb

instance Database TestDb where
  type Tables TestDb = '[Post Hask]

instance Table TestDb (Post Hask) where
  type HasDefault (Post Hask) = '["id"]
  type TableName (Post Hask)  = "posts"
  type PrimaryKey (Post Hask) = '["id"]

  type Check (Post Hask) = '[ 'CheckOn '["title"] "notnull"
                            , 'CheckOn '["body"] "notnull"
                            , 'CheckOn '["created_at"] "notnull"
                            , 'CheckOn '["updated_at"] "notnull"
                            ]

  defaults = dbDefaults (def @"id" @(Post Hask) (PostId 1) :& end)
  checks = dbChecks (check @"notnull" (\f -> f == f) :& end)

findById :: PostId -> Query (Post Op)
findById postId = proc () -> do
  post <- query (Tab @TestDb @Post) -< ()
  restrict -< (id post) .== constant postId
  returnA -< post

postsTable :: Tab TestDb Post
postsTable = Tab

