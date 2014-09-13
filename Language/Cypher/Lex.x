{
{-# OPTIONS_GHC -w #-}
module Language.Cypher.Lex (runLex, Token (..), Side (..)) where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$eol   = [\n]

tokens :-

  $eol                          ;
  $white+                       ;
  "//".*                        ;
  $digit+                       { Int . read }
  [\+\-\*\=]                    { Operator }
  \.                            { const Dot }
  :                             { const Colon }
  \,                            { const Comma }
  \$([^\$]+)\$                  { AntiQuote }
  \(                            { const $ Paren L }
  \)                            { const $ Paren R }
  \[                            { const $ Bracket L }
  \]                            { const $ Bracket R }
  \{                            { const $ Brace L }
  \}                            { const $ Brace R }
  \" ([^\"])* \"                { String }
  $alpha [$alpha $digit \_ \']* { Name }

{
data Token =
    Int Integer
  | Float Rational
  | String String
  | Name String
  | Operator String
  | Paren Side
  | Bracket Side
  | Brace Side
  | Dot
  | Colon
  | Comma
  | AntiQuote String
  deriving Show

data Side = L | R deriving Show

runLex = alexScanTokens  

}  
