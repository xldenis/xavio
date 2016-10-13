module Config where

import Database.PostgreSQL.Simple as PGS
import Database.Transaction as DB

data Config = Config
  { port :: Int
  , database :: DB.Config ()
  }
