{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, DeriveGeneric, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, OverloadedLabels, Arrows, TypeApplications, UndecidableInstances, FlexibleContexts #-}
module Api.Post where
import Prelude hiding (id)

import Data.Aeson
import Data.Profunctor
import Data.Profunctor.Product.Default hiding (def)
import qualified Data.Profunctor.Product.Default as P (def)

import Opaleye

import Data.Text as T
import Data.Time.Clock

import GHC.Generics

import Control.Arrow
import Control.Monad.Reader

import Web.HttpApiData

import Database

import           Database.PostgreSQL.Simple.FromField

newtype PostId = PostId Int
  deriving (Generic, FromJSON, ToJSON, FromHttpApiData, FromField)

instance Default Constant PostId (Column PGInt4) where
  def = Constant (pgInt4 . unPost)
    where unPost (PostId i) = i

instance QueryRunnerColumnDefault PGInt4 PostId where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

data Post f = Post
  { id :: TableField f PostId PGInt4 NN Opt
  , title :: TableField f Text PGText NN Req
  , body  :: TableField f Text PGText NN Req
  , created_at :: TableField f UTCTime PGTimestamptz NN Req
  , updated_at :: TableField f UTCTime PGTimestamptz NN Req
  } deriving (Generic)

instance ( Profunctor p
         , Applicative (p (Post f))
         , Default p (TableField f PostId PGInt4 NN Opt) (TableField g PostId PGInt4 NN Opt)
         , Default p (TableField f Text PGText NN Req) (TableField g Text PGText NN Req)
         , Default p (TableField f Text PGText NN Req) (TableField g Text PGText NN Req)
         , Default p (TableField f UTCTime PGTimestamptz NN Req) (TableField g UTCTime PGTimestamptz NN Req)
         , Default p (TableField f UTCTime PGTimestamptz NN Req) (TableField g UTCTime PGTimestamptz NN Req)
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

findById :: PostId -> Query (Post Op)
findById postId = proc () -> do
  post <- queryTable (postsTable) -< ()
  restrict -< (id post) .== constant postId
  returnA -< post

postsTable :: Table (Post W) (Post Op)
postsTable = Table "posts_table" (Post <$> lmap id (optional "id")
                                       <*> lmap title (required "title")
                                       <*> lmap body (required "body")
                                       <*> lmap created_at (required "created_at")
                                       <*> lmap updated_at (required "updated_at")
                                 )

