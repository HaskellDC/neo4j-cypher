{

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Cypher.Parse where

import Data.Char (toLower)
import Language.Cypher
import Language.Cypher.Lex

import Language.Haskell.TH hiding (QReturn)

import Language.Haskell.Meta.Parse.Careful
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
  ','              { Comma }
  int              { Int $$ }
  antiquoted       { AntiQuote $$ }
  match            { Name x | map toLower x == "match"  }
  return           { Name x | map toLower x == "return" }
  limit            { Name x | map toLower x == "limit"  }
  order            { Name x | map toLower x == "order"  }
  by               { Name x | map toLower x == "by"     }
  skip             { Name x | map toLower x == "skip"   }
  union            { Name x | map toLower x == "union"  }
  all              { Name x | map toLower x == "all"    }
  end              { Name x | map toLower x == "end"    }
  case             { Name x | map toLower x == "case"   }
  when             { Name x | map toLower x == "when"   }
  then             { Name x | map toLower x == "then"   }
  else             { Name x | map toLower x == "else"   }
  name             { Name $$ }

%%

Query :: { Q Exp }
  : Match return RetClause OrderBy Skip Limit {  
    [| QReturn $($1) $($3) $($4) $5 $6 |] }
  | Query union Query     { [| QUnion False $($1) $($3) |] }
  | Query union all Query { [| QUnion True $($1) $($4) |] } 

Match :: { Q Exp }
  :                { [| Nothing    |] }
  | match Pattern  { [| Just $($2) |] }

Limit :: { Maybe Int }
  :             { Nothing }
  | limit int   { Just (fromIntegral $2) }

Skip :: { Maybe Int }
  :             { Nothing }
  | skip int    { Just (fromIntegral $2) }

OrderBy :: { Q Exp }
  :               { [| Nothing :: Maybe (E Number) |] }
  | order by Exp  { [| Just $($3) |] }
  
Pattern :: { Q Exp }
  : Node  { $1 }

Node :: { Q Exp }
  : name { [| PNode (Just (EIdent $1)) [] [] |] }
  | '(' Node ')'   { $2 }

RetClause :: { Q Exp }
  : Exp                  { [| $($1) ::: HNil |] }
  | Exp ',' RetClause    { [| $($1) ::: $($3) |] }

Exp :: { Q Exp }
  : name                   { [| EIdent $1 |] }
  | name '.' name          { [| EProp (EIdent $1) $3 |] }
  | int                    { [| EInt (fromIntegral ($1 :: Integer)) |] }
  | '(' Exp ')'            { $2 }
  | SCase                  { $1 }
  | GCase                  { $1 }
  | antiquoted             { case parseExp $1 of Right e -> return e }

SCase :: { Q Exp }
  : case Exp Whens Else end { [| ESCase $($2) $($3) $($4) |] }

GCase :: { Q Exp }
  : case Whens Else end { [| EGCase $($2) $($3) |] }

Whens :: { Q Exp }
  : When Whens  { [| $($1) : $($2) |] }
  | When        { [| [ $($1) ] |] }

When :: { Q Exp }
  : when Exp then Exp    { [| ( $($2), $($4) ) |] }


Else :: { Q Exp }
  :            { [| Nothing |] }
  | else Exp   { [| Just $($2) |] }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

}
