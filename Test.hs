{-# LANGUAGE TemplateHaskell, DataKinds, OverloadedStrings #-}

module Test where

import Language.Cypher
import Language.Cypher.Lex
import Language.Cypher.Parse

import Database.Neo4j (queryDB)
import Database.Neo4j.Types (Server (..))

query :: Query '[Number]
query = $(parse $ runLex "return case 1 when 0 then 3 else 2 end limit 1")

main :: IO ()
main = do
  x <- queryDB localServer query
  print x
  where
  localServer = Server "http://neo4j.skeweredrook.com:7474/db/data/transaction/commit"
