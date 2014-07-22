{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
import Test.Tasty.HUnit ((@?=), Assertion, assertBool, assertFailure, testCase)
import Test.Tasty.TH (defaultMainGenerator)
import Control.Exception (Exception, handleJust)
import Control.Monad (guard)

import Database.Neo4j
import Database.Neo4j.Types

localServer :: Server
localServer = Server "http://127.0.0.1:7474/db/data/transaction/commit"

simpleQuery :: Query
simpleQuery = "RETURN 1"

--------------------------------------------------

assertException :: (Exception e, Eq e) => e -> IO a -> IO ()
assertException ex action =
  handleJust isWanted (const $ return ()) $ do
    action
    assertFailure $ "Expected exception: " ++ show ex ++ "\nActual exception"
    where isWanted = guard . (== ex)

case_queryBadServer :: Assertion
case_queryBadServer = 
  assertException (userError badLookupMessage) (queryDBRaw badServer simpleQuery)
  where 
    badServer = Server "http://wutwut"
    badLookupMessage = "openTCPConnection: host lookup failure for \"wutwut\""

case_queryRaw :: Assertion
case_queryRaw = do
  res <- queryDBRaw localServer simpleQuery
  res @?= Right "{\"results\":[{\"columns\":[\"1\"],\"data\":[{\"row\":[1]}]}],\"errors\":[]}"

--case_querySingle :: Assertion
--case_querySingle = do
--  res <- queryDB localServer simpleQuery
--  return ()
--  res @?= Right [CTInt 1]

main :: IO ()
main = $defaultMainGenerator
