{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE FlexibleInstances #-}

module API where
import Prelude (IO, (.))
import Control.Monad.Except
import Data.Text as T

import Database.PostgreSQL.Simple as PGS
import Opaleye

import Data.Aeson

import Servant

type Responders = '[JSON]
type Requesters = '[JSON]

type Response t a = HandlerM (TaggedResp t a)

type HandlerM = ExceptT ServantErr IO

data Method = Index | Show | New | Create | Edit | Update | Destroy

newtype TaggedResp (t :: Method) a = T a

instance ToJSON a => ToJSON (TaggedResp t a) where
  toJSON (T a) = toJSON a

type APIFor a i =
       Get Responders (TaggedResp Index [a]) -- list 'a's
  :<|> ReqBody Requesters a :> PostCreated Responders (TaggedResp Create a) -- add an 'a'
  :<|> Capture "id" i :>
    (    Get Responders (TaggedResp Show a)
    :<|> ReqBody Requesters a :> PutNoContent Responders NoContent
    :<|> DeleteNoContent Responders NoContent
    )

serverFor :: Response Index [a] -- handler for listing of 'a's
          -> (i -> Response Show a) -- handler for viewing an 'a' given its identifier of type 'i'
          -> (a -> Response Create a) -- handler for adding an 'a'
          -> (i -> a -> HandlerM NoContent) -- updating an 'a' with given id
          -> (i -> HandlerM NoContent) -- deleting an 'a' given its id
          -> Server (APIFor a i)

serverFor index show create update destroy = index :<|> create :<|> (\id -> show id :<|> update id :<|> destroy id)

pgText :: Text -> Column PGText
pgText = pgString . T.unpack
