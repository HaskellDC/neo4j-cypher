{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
import Test.Tasty.HUnit ((@?=), Assertion, testCase)
import Test.Tasty.TH (defaultMainGenerator)

import Language.Cypher
import Language.Cypher.Quasiquoter

-- -- fails to parse
-- case_returnSign :: Assertion
-- case_returnSign =
--   writeQuery [cypher| RETURN sign(-3)|] @?= "RETURN sign(-3)"

case_simpleMatchExample :: Assertion
case_simpleMatchExample = 
  writeQuery [cypher| MATCH (n) RETURN n|] @?= "MATCH (n) RETURN n"

case_matchComma :: Assertion
case_matchComma = 
  writeQuery [cypher| MATCH (n), (m) RETURN n, m|] @?= "MATCH (n), (m) RETURN n, m"

case_matchSimpleRelPattern :: Assertion
case_matchSimpleRelPattern = 
  writeQuery [cypher| MATCH (n)--(m) RETURN n, m|] @?= "MATCH (n)--(m) RETURN n, m"

case_matchSimpleRelLeftPattern :: Assertion
case_matchSimpleRelLeftPattern = 
  writeQuery [cypher| MATCH (n)<--(m) RETURN n, m|] @?= "MATCH (n)<--(m) RETURN n, m"

case_matchSimpleRelRightPattern :: Assertion
case_matchSimpleRelRightPattern = 
  writeQuery [cypher| MATCH (n)-->(m) RETURN n, m|] @?= "MATCH (n)-->(m) RETURN n, m"

-- -- fails to parse
-- case_matchTypedRelRightPattern :: Assertion
-- case_matchTypedRelRightPattern = 
--   writeQuery [cypher| MATCH (n)-[:TYPE]->(m) RETURN n, m|] @?= "MATCH (n)-[:TYPE]->(m) RETURN n, m"

-- -- fails to parse
-- case_matchTypedRelPattern ::Assertion
-- case_matchTypedRelPattern =
--   writeQuery [cypher| MATCH (n)-[:TYPE]-(m) RETURN n, m|] @?= "MATCH (n)-[:TYPE]-(m) RETURN n, m"

case_matchRelCommaPattern :: Assertion
case_matchRelCommaPattern = 
  writeQuery [cypher| MATCH (n)-->(m), (o) RETURN n, m|] @?= "MATCH (n)-->(m), (o) RETURN n, m"

main :: IO ()
main = $defaultMainGenerator
