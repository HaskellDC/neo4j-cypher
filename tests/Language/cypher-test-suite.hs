{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
import Test.Tasty.HUnit ((@?=), Assertion, testCase)
import Test.Tasty.TH (defaultMainGenerator)

import Language.Cypher

case_simpleMatchExample :: Assertion
case_simpleMatchExample = 
  show (simpleMatch (ENode (Just n) [] []) [n]) @?= "MATCH (n) RETURN n"
  where
    n = EIdent "n"

case_simpleMatchExample2 :: Assertion
case_simpleMatchExample2 =
  show ex2 @?= "MATCH (me)-[:KNOWS*1..2]-(remote_friend) RETURN remote_friend.name" 
  where
    ex2 :: Query
    ex2 = simpleMatch
      (ERel left right (ManyEdges range) RelBoth [] ["KNOWS"])
      [EProp remote_friend "name"]
    range = Range (Just 1) (Just 2)
    remote_friend = EIdent "remote_friend"
    me = EIdent "me"
    left = ENode (Just me) [] []
    right = ENode (Just remote_friend) [] []

main :: IO ()
main = $defaultMainGenerator
