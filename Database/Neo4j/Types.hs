module Database.Neo4j.Types
  ( 
    Server(..)
  ) where

import Data.Text (Text)

-- | Server location.
data Server = Server
  { serverURI :: !Text
  -- ^ full URI to server API (db/data/) -- http://host:port/db/data/
  } deriving Show
