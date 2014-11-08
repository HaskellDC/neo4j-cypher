{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
import Test.Tasty.HUnit ((@?=), Assertion, testCase)
import Test.Tasty.TH (defaultMainGenerator)

import Language.Cypher
import Language.Cypher.Quasiquoter

case_simpleMatchExample :: Assertion
case_simpleMatchExample = 
  writeQuery [cypher| MATCH (n) RETURN n|] @?= "MATCH (n) RETURN n"

main :: IO ()
main = $defaultMainGenerator
