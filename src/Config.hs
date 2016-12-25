module Config where

import Database.PostgreSQL.Simple as PGS

data Config = Config
  { port :: Int
  , database :: PGS.Connection
  }
