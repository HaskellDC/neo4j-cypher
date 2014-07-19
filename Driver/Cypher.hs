{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import qualified Network.HTTP as H
import Network.URI(parseURI,URI)
import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as C


dbServer = fromJust $ parseURI "http://neo4j.skeweredrook.com:7474/db/data/transaction/commit"
qs = "match (n:Position) return n.score, n.fen limit 10"

type Query = String
type Result = IO Value
type BS = C.ByteString

createRequest :: Query -> Value
createRequest query = 
	object ["statements" .= [
			object [
				"statement" .= query
				--"parameters" .= object []
			]
		]
	]


query :: URI -> Query -> IO (H.Result String)
query uri query =  fmap (H.rspBody) (H.simpleHTTP request)
	where 
		body = encode $ createRequest query
		headers = [H.mkHeader H.HdrAccept "application/json"
		  		  ,H.mkHeader H.HdrContentType "application/json"
		    	  ,H.mkHeader H.HdrContentLength (show $ C.length body)
		  		  ]
		request = H.Request uri H.POST headers body


main = print query dbServer qs

