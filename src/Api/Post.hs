{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, DeriveGeneric, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, DuplicateRecordFields, Arrows, TypeApplications, UndecidableInstances, FlexibleContexts #-}
module Api.Post where
import Prelude hiding (id)

import Data.Aeson
import Data.Profunctor
import Data.Profunctor.Product.Default

import Database.Migration hiding (Column, def, Col)
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
  , createdAt :: Col f "created_at" UTCTime
  , updatedAt :: Col f "updated_at" UTCTime
  } deriving (Generic)

instance ( Profunctor p
         , Applicative (p (Post f))
         , Default p (Col f "id" PostId) (Col g "id" PostId)
         , Default p (Col f "title" Text) (Col g "title" Text)
         , Default p (Col f "body" Text) (Col g "body" Text)
         , Default p (Col f "created_at" UTCTime) (Col g "created_at" UTCTime)
         , Default p (Col f "updated_at" UTCTime) (Col g "updated_at" UTCTime)
         ) => Default p (Post f) (Post g) where
  def = Post <$> lmap id def
             <*> lmap title def
             <*> lmap body def
             <*> lmap createdAt def
             <*> lmap updatedAt def

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

findById :: PostId -> Query (Post Op)
findById postId = proc () -> do
  post <- query (Tab @TestDb @Post) -< ()
  restrict -< (id post) .== constant postId
  returnA -< post

test :: PG [Post Hask]
test = getAll (findById (PostId 1))