{-# LANGUAGE OverloadedStrings, KindSignatures, DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module Database.Neo4j where

import Control.Applicative ((<$>), (<*>))

import Database.Neo4j.Types
import Language.Cypher

import qualified Network.HTTP as H
import qualified Network.Stream as H
import Network.URI (parseURI)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Aeson ((.=), object, (.:))
import qualified Data.Aeson as A
import Data.Aeson.Types (parse)
import qualified Data.Text as T
import qualified Data.Vector as V

type QueryString = String

data QueryResult (xs :: [CType]) = QueryResult
  { columns :: [T.Text]
  , rows    :: [HList Value xs]
  }

instance (Show (Value a), Show (HList Value as)) => Show (QueryResult (a ': as)) where
  show (QueryResult c r) = "QueryResult " ++ show c ++ " " ++ show r

instance (Eq (Value a), Eq (HList Value as)) => Eq (QueryResult (a ': as)) where
  QueryResult c1 r1 == QueryResult c2 r2 = c1 == c2 && r1 == r2

createRequest :: QueryString -> A.Value
createRequest query = 
  object ["statements" .= [
      object [
        "statement" .= query
        --"parameters" .= object []
      ]
    ]
  ]

queryDBRaw :: Server -> QueryString -> IO (H.Result B.ByteString)
queryDBRaw server query = fmap (fmap H.rspBody) (H.simpleHTTP request)
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




queryDB :: ConvertL xs => Server -> Query xs -> IO (Either String (QueryResult xs))
queryDB server = fmap f . queryDBRaw server . writeQuery where
  f (Left connError) = Left $ show connError 
  f (Right val) = case (A.decode val :: Maybe A.Value) of
    Nothing -> Left "Error reading JSON"
    Just json -> case parse parseOutput json of
      A.Error str -> Left str
      A.Success (cols, dat) -> case mapM convertl dat of
        Just xs -> Right (QueryResult cols xs)
        Nothing -> Left "Type mismatch"
  parseOutput (A.Object json) = do
    A.Array results <- json .: "results"
    let A.Object res = V.head results
    (,) <$> res .: "columns" <*> (mapM parseRow =<< res .: "data")
  parseOutput _ = fail "Expected object"
  parseRow (A.Object row) = A.parseJSON =<< row .: "row"
  parseRow _ = fail "Expected row"



