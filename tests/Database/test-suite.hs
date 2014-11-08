{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
import Test.Tasty.HUnit ((@?=), Assertion, testCase)
import Test.Tasty.TH (defaultMainGenerator)
--import Control.Exception (Exception, handleJust)
--import Control.Monad (guard)

import Database.Neo4j
import Database.Neo4j.Types

import Language.Cypher
import Language.Cypher.Quasiquoter

localServer :: Server
localServer = Server "http://127.0.0.1:7474/db/data/transaction/commit"

simpleQuery :: QueryString
simpleQuery = "RETURN 1"

--------------------------------------------------

--isLeft :: Either a b -> Bool
--isLeft (Left _) = True
--isLeft (Right _) = False

--assertException :: (Exception e, Eq e) => e -> IO a -> IO ()
--assertException ex action =
--  handleJust isWanted (const $ return ()) $ do
--    _ <- action
--    assertFailure $ "Expected exception: " ++ show ex ++ "\nActual exception"
--    where isWanted = guard . (== ex)

-- this fails differently on mac and ubuntu; need to figure out how to really test exceptions
-- commenting out for now
--case_queryBadServer :: Assertion
--case_queryBadServer = 
--  assertException (userError badLookupMessage) (queryDBRaw badServer simpleQuery)
--  where 
--    badServer = Server "http://wutwut"
--    badLookupMessage = "openTCPConnection: host lookup failure for \"wutwut\""

case_queryRaw :: Assertion
case_queryRaw = do
  res <- queryDBRaw localServer [] simpleQuery
  res @?= Right "{\"results\":[{\"columns\":[\"1\"],\"data\":[{\"row\":[1]}]}],\"errors\":[]}"

-- TODO add an order by to ensure consistency of this result
case_queryDBTest :: Assertion
case_queryDBTest = do
  res <- queryDB localServer [] query :: IO (Either String (QueryResult [Str, Number]))
  res @?= Right (QueryResult ["m.fen","m.score"] 
   [VStr "8/1R1p1pkp/3Pr1p1/3Q4/4N3/r3PKPq/7P/8 b - - 7 33" ::: VNum (-1500.0) ::: HNil,
    VStr "8/1R1p1pkp/3Pr1p1/3Q4/4N3/r3PKP1/7P/5q2 w - - 8 34" ::: VNum (-1143.0) ::: HNil,
    VStr "1R4k1/3p1p1p/3Pr1p1/3Q4/4N3/r3PKPq/7P/8 b - - 5 32" ::: VNum (-651.0) ::: HNil,
    VStr "6k1/8/3p3b/1p1P2p1/6P1/4P2P/1p1n4/5QK1 w - - 1 42" ::: VNum (-591.0) ::: HNil,
    VStr "6k1/8/3p3b/1p1P2p1/2n3P1/3QP2P/1p6/r4RK1 b - - 2 40" ::: VNum (-566.0) ::: HNil])
  where
  query = [cypher|MATCH (m:Position) RETURN m.fen, m.score ORDER BY m.score LIMIT 5|]

main :: IO ()
main = $defaultMainGenerator
