{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
import Test.Tasty.HUnit ((@?=), Assertion, testCase)
import Test.Tasty.TH (defaultMainGenerator)

import Language.Cypher

case_simpleMatchExample :: Assertion
case_simpleMatchExample = 
  show (simpleMatch (PNode (Just n) [] []) (RetE n ::: HNil)) @?= "MATCH (n) RETURN n"
  where
    n = EIdent "n"

case_simpleMatchExample2 :: Assertion
case_simpleMatchExample2 =
  show ex2 @?= "MATCH (me)-[:KNOWS*1..2]-(remote_friend) RETURN remote_friend.name" 
  where
    ex2 :: Query '[Str]
    ex2 = simpleMatch
      (PRel left right Nothing (Just range) RelBoth [] ["KNOWS"])
      (RetE (EProp remote_friend "name") ::: HNil)
    range = Range (Just 1) (Just 2)
    remote_friend = EIdent "remote_friend"
    me = EIdent "me"
    left = PNode (Just me) [] []
    right = PNode (Just remote_friend) [] []

main :: IO ()
main = $defaultMainGenerator
