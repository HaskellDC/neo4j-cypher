{-# LANGUAGE OverloadedStrings #-}

module Database.Neo4j where

import Data.Aeson
import qualified Network.HTTP as H
import qualified Network.Stream as H
import Network.URI (parseURI)
import qualified Data.ByteString.Lazy.Char8 as B
import Database.Neo4j.Types
import qualified Data.Text as T

dbServer :: Server
dbServer = Server "http://localhost:7474/db/data/transaction/commit"

type Query = String
 
qs :: Query
qs = "return 1"

createRequest :: Query -> Value
createRequest query = 
  object ["statements" .= [
      object [
        "statement" .= query
        --"parameters" .= object []
      ]
    ]
  ]


queryDB :: Server -> Query -> IO (H.Result B.ByteString)
queryDB server query = fmap (fmap (H.rspBody)) (H.simpleHTTP request)
  where 
    Just uri = parseURI $ T.unpack $ serverURI server
    body = encode $ createRequest query
    headers = [ H.mkHeader H.HdrAccept "application/json"
              , H.mkHeader H.HdrContentType "application/json"
              , H.mkHeader H.HdrContentLength (show $ B.length body)
              ]
    request = H.Request uri H.POST headers body

