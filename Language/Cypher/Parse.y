{

{-# LANGUAGE DataKinds #-}

module Language.Cypher.Parse where

import Data.Char (toLower)
import Language.Cypher
import Language.Cypher.Lex
}

%name parse
%tokentype { Token }
%error { parseError }

%token
  '('              { Paren L }
  ')'              { Paren R }
  '{'              { Brace L }
  '}'              { Brace R }
  '['              { Bracket L }
  ']'              { Bracket R }
  '.'              { Dot }
  ':'              { Colon }
  int              { Int $$ }
  match            { Name x | map toLower x == "match"  }
  return           { Name x | map toLower x == "return" }
  limit            { Name x | map toLower x == "limit"  }
  name             { Name $$ }

%%

Query
  : match Pattern return RetClause limit int {  
    QMatch $2 $4 (Nothing :: Maybe (E Number)) Nothing
      (Just (fromIntegral $6)) }

Pattern
  : Node  { $1 }

Node
  : name { PNode (Just (EIdent $1)) [] [] }
  | '(' Node ')'   { $2 }

RetClause
  : Exp { $1 ::: HNil }

Exp
  : name                   { EIdent $1 }
  | '(' Exp ')'            { $2 }


{

parseError :: [Token] -> a
parseError _ = error "Parse error"

}
