{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
import Test.Tasty.HUnit ((@?=), Assertion, testCase)
import Test.Tasty.TH (defaultMainGenerator)
--import Control.Exception (Exception, handleJust)
--import Control.Monad (guard)

import Database.Neo4j
import Database.Neo4j.Types

import Language.Cypher

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
  res <- queryDBRaw localServer simpleQuery
  res @?= Right "{\"results\":[{\"columns\":[\"1\"],\"data\":[{\"row\":[1]}]}],\"errors\":[]}"

case_queryDBTest :: Assertion
case_queryDBTest = do
  res <- queryDB localServer query :: IO (Either String (QueryResult [Str, Number]))
  res @?= Right (QueryResult ["r.move","(n.score) - (m.score)"] 
    [VStr "b6d5" ::: VNum 773.0 ::: HNil,VStr "g7d4" ::: VNum 549.0 ::: HNil
    ,VStr "b6a8" ::: VNum 950.0 ::: HNil,VStr "g7e5" ::: VNum 520.0 ::: HNil
    ,VStr "b6c8" ::: VNum 463.0 ::: HNil,VStr "b6a4" ::: VNum 327.0 ::: HNil
    ,VStr "d7d5" ::: VNum 218.0 ::: HNil,VStr "a6a5" ::: VNum 156.0 ::: HNil
    ,VStr "b7a7" ::: VNum 154.0 ::: HNil,VStr "b7c7" ::: VNum 154.0 ::: HNil])
  where
  query = QMatch pattern ret (Nothing :: Maybe (E Number)) Nothing (Just 10)
  pattern = PRel (node m) (node n) (OneEdge (Just r)) RelRight [] ["NEXT"]
  ret = EProp r "move" ::: EMinus (EProp n "score") (EProp m "score") ::: HNil
  node x = PNode (Just x) [] ["Position"] 
  [m, n, r] = map EIdent ["m", "n", "r"]

main :: IO ()
main = $defaultMainGenerator
