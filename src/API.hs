{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DataKinds       #-}

module API where

import Servant
import Control.Monad.Trans.Either
import Database.PostgreSQL.Simple as PGS

type Responders = '[JSON]

type Response = EitherT ServantErr IO

type APIFor a i =
       Get Responders [a] -- list 'a's
  :<|> ReqBody Responders a :> Post '[] () -- add an 'a'
  :<|> Capture "id" i :>
      (  Get Responders a
    :<|> ReqBody Responders a :> Put '[] ()
    :<|> Delete '[] ()
      )

serverFor :: Response [a] -- handler for listing of 'a's
          -> (i -> Response a) -- handler for viewing an 'a' given its identifier of type 'i'
          -> (a -> Response ()) -- handler for adding an 'a'
          -> (i -> a -> Response ()) -- updating an 'a' with given id
          -> (i -> Response ()) -- deleting an 'a' given its id
          -> Server (APIFor a i)

serverFor index show create update destroy = index :<|> create :<|> (\id -> show id :<|> update id :<|> destroy id)

