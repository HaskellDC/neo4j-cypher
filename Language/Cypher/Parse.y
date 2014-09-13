{

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Cypher.Parse where

import Data.Char (toLower)
import Language.Cypher
import Language.Cypher.Lex

import Language.Haskell.TH hiding (QReturn, Match)

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
  '+'              { Operator "+" }
  '-'              { Operator "-" }
  '*'              { Operator "*" }
  '/'              { Operator "/" }
  '=~'             { Operator "=~" }
  int              { Int $$ }
  antiquoted       { AntiQuote $$ }
  match            { Name x | map toLower x == "match"    }
  as               { Name x | map toLower x == "as"       }
  where            { Name x | map toLower x == "where"    }
  optional         { Name x | map toLower x == "optional" }
  return           { Name x | map toLower x == "return"   }
  limit            { Name x | map toLower x == "limit"    }
  order            { Name x | map toLower x == "order"    }
  by               { Name x | map toLower x == "by"       }
  skip             { Name x | map toLower x == "skip"     }
  union            { Name x | map toLower x == "union"    }
  all              { Name x | map toLower x == "all"      }
  end              { Name x | map toLower x == "end"      }
  case             { Name x | map toLower x == "case"     }
  when             { Name x | map toLower x == "when"     }
  then             { Name x | map toLower x == "then"     }
  else             { Name x | map toLower x == "else"     }
  name             { Name $$ }

%left '+' '-'
%left '*' '/'

%%

Query :: { Q Exp }
  : Matches return RetClause OrderBy Skip Limit {  
    [| QReturn $($1) $($3) $($4) $5 $6 |] }
  | Query union Query     { [| QUnion False $($1) $($3) |] }
  | Query union all Query { [| QUnion True $($1) $($4) |] } 

MatchType :: { Q Exp }
  : match { [| RequiredMatch |] }
  | optional match { [| OptionalMatch |] }

Matches :: { Q Exp }
  :   { [| [] |] }
  | Match Matches { [| $($1) : $($2) |] }

Match :: { Q Exp }
  : MatchType Pattern MWhere    {  [| Match $($1) $($2) $($3) |] }

MWhere :: { Q Exp }
  :                      { [| Nothing |] }
  | Where                { [| Just $($1) |] }

Where :: { Q Exp }
  : where Exp { [| Where $($2) |] }

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
  | Exp '+' Exp            { [| EPlus $($1) $($3) |] }
  | Exp '-' Exp            { [| EMinus $($1) $($3) |] }
  | Exp '*' Exp            { [| ETimes $($1) $($3) |] }
  | Exp '/' Exp            { [| EDiv $($1) $($3) |] }
  | Exp '=~' Exp           { [| ERegExpEQ $($1) $($3) |] }
  | SCase                  { $1 }
  | GCase                  { $1 }
  | antiquoted             { case parseExp $1 of Right e -> return e; Left err -> error err }

SCase :: { Q Exp }
  : case Exp Whens Else end { [| ESCase $($2) $($3) $($4) |] }

GCase :: { Q Exp }
  : case Whens Else end { [| EGCase $($2) $($3) |] }

Whens :: { Q Exp }
  : When Whens  { [| $($1) : $($2) |] }
  | When        { [| [ $($1) ] |] }

When :: { Q Exp }
  : when Exp then Exp    { [| ( $($2), $($4) ) |] }

As :: { Q Exp }
  : Exp as name { [| EAs $($1) $3 |] }

Else :: { Q Exp }
  :            { [| Nothing |] }
  | else Exp   { [| Just $($2) |] }

{

parseError :: [Token] -> a
parseError toks = error ("Parse error: " ++ show toks)

}
