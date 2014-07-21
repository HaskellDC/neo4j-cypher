{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
import Test.Tasty.HUnit
import Test.Tasty.TH

import Database.Neo4j
import Database.Neo4j.Types

localServer :: Server
localServer = Server "http://127.0.0.1:7474/db/data/transaction/commit"

simpleQuery :: Query
simpleQuery = "RETURN 1"

--------------------------------------------------

case_query :: Assertion
case_query = do
  res <- queryDB localServer simpleQuery
  res @?= Right "{\"results\":[{\"columns\":[\"1\"],\"data\":[{\"row\":[1]}]}],\"errors\":[]}"

main :: IO ()
main = $defaultMainGenerator
