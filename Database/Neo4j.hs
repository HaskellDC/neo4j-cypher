{-# LANGUAGE OverloadedStrings #-}

module Database.Neo4j 
  where

import qualified Network.HTTP as H
import qualified Network.Stream as H
import Network.URI (parseURI)
import qualified Data.ByteString.Lazy.Char8 as B
import Database.Neo4j.Types
import Data.Aeson ((.=), object)
import qualified Data.Aeson as A
import qualified Data.Text as T

type Query = String
 
createRequest :: Query -> A.Value
createRequest query = 
  object ["statements" .= [
      object [
        "statement" .= query
        --"parameters" .= object []
      ]
    ]
  ]

queryDBRaw :: Server -> Query -> IO (H.Result B.ByteString)
queryDBRaw server query = fmap (fmap (H.rspBody)) (H.simpleHTTP request)
  where 
    Just uri = parseURI $ T.unpack $ serverURI server
    body = A.encode $ createRequest query
    headers = [ H.mkHeader H.HdrAccept "application/json"
              , H.mkHeader H.HdrContentType "application/json"
              , H.mkHeader H.HdrContentLength (show $ B.length body)
              ]
    request = H.Request uri H.POST headers body

--queryDB :: Server -> Query -> IO (H.Result Value)
--queryDB s q = (d (queryDBRaw s q)) ^. AL.key (pack "results") . AL.key (pack "data") . AL.key (pack "row") . AL.nth 0
--  where
--    d str = A.decode str :: IO (H.Result Value)
