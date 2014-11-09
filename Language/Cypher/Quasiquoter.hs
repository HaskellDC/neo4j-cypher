module Language.Cypher.Quasiquoter where

import Language.Cypher.Lex
import Language.Cypher.Parse

import Language.Haskell.TH.Quote


cypher :: QuasiQuoter
cypher = QuasiQuoter 
  { quoteExp = parse . runLex 
  , quotePat = error "No pattern matching support for cypher quasiquoter" 
  , quoteType =  undefined
  , quoteDec = undefined 
  }

