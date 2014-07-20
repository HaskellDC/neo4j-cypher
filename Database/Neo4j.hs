{-# LANGUAGE OverloadedStrings #-}

module Database.Neo4j where

import Data.Aeson
import qualified Network.HTTP as H
import qualified Network.Stream as H
import Network.URI (parseURI, URI)
import qualified Data.ByteString.Lazy.Char8 as B

dbServer :: URI
Just dbServer = parseURI "http://neo4j.skeweredrook.com:7474/db/data/transaction/commit"

type Query = String
 
qs :: Query
qs = "match (n:Position) return n.score, n.fen limit 10"

createRequest :: Query -> Value
createRequest query = 
  object ["statements" .= [
      object [
        "statement" .= query
        --"parameters" .= object []
      ]
    ]
  ]


queryDB :: URI -> Query -> IO (H.Result B.ByteString)
queryDB uri query = fmap (fmap (H.rspBody)) (H.simpleHTTP request)
  where 
    body = encode $ createRequest query
    headers = [ H.mkHeader H.HdrAccept "application/json"
              , H.mkHeader H.HdrContentType "application/json"
              , H.mkHeader H.HdrContentLength (show $ B.length body)
              ]
    request = H.Request uri H.POST headers body


example :: IO ()
example = do
  Right res <- queryDB dbServer qs
  B.putStrLn res

