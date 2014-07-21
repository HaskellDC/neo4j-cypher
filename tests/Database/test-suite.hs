{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
import Test.Tasty.HUnit
import Test.Tasty.TH

import Database.Neo4j

simpleQuery :: Query
simpleQuery = "RETURN 1"

case_query :: Assertion
case_query = do
  res <- queryDB dbServer simpleQuery
  res @?= Right "{\"results\":[{\"columns\":[\"1\"],\"data\":[{\"row\":[1]}]}],\"errors\":[]}"

main :: IO ()
main = $defaultMainGenerator
