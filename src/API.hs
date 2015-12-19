{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DataKinds       #-}

module API where

import Control.Monad.Trans.Either
import Data.Text as T

import Database.PostgreSQL.Simple as PGS
import Opaleye

import Servant


type Responders = '[JSON]
type Requesters = '[JSON]

type Response = EitherT ServantErr IO

type APIFor a i =
       Get Responders [a] -- list 'a's
  :<|> ReqBody Requesters a :> Post Responders a -- add an 'a'
  :<|> Capture "id" i :>
      (  Get Responders a
    :<|> ReqBody Requesters a :> Put '[] ()
    :<|> Delete '[] ()
      )

serverFor :: Response [a] -- handler for listing of 'a's
          -> (i -> Response a) -- handler for viewing an 'a' given its identifier of type 'i'
          -> (a -> Response a) -- handler for adding an 'a'
          -> (i -> a -> Response ()) -- updating an 'a' with given id
          -> (i -> Response ()) -- deleting an 'a' given its id
          -> Server (APIFor a i)

serverFor index show create update destroy = index :<|> create :<|> (\id -> show id :<|> update id :<|> destroy id)

pgText :: Text -> Column PGText
pgText = pgString . T.unpack
