module Config where

import Database.PostgreSQL.Simple as PGS

data Config = Config
  { port :: Int
  , conn :: PGS.Connection
  }
