{-# LANGUAGE QuasiQuotes, DataKinds, OverloadedStrings #-}

module Test where

import Language.Cypher
import Language.Cypher.Quasiquoter

import Database.Neo4j (queryDB)
import Database.Neo4j.Types (Server (..))

query :: Query '[Number]
query = [cypher| return case $ x - 1 $ when 0 then $ x $ else 2 end limit 1|]
  where 
  x :: E Number
  x = EProp (EIdent "actor") "name"

main :: IO ()
main = do
  x <- queryDB localServer query
  print x
  where
  localServer = Server "http://neo4j.skeweredrook.com:7474/db/data/transaction/commit"
