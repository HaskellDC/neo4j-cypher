{-# LANGUAGE GADTs, DataKinds, KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}

module Language.Cypher where

import Data.List (intersperse)
import Data.Monoid (Monoid (..), (<>))
import Data.String (IsString (fromString))

data CType = 
    Number | Property | Boolean | Str
  | Identifier
  | Collection CType
  | Pattern

class Case (a :: CType)
class EOrd (a :: CType)
instance EOrd Number

newtype Label = Label String deriving (IsString)
newtype RelType = RelType String deriving (IsString)

data Range = Range (Maybe Int) (Maybe Int)

data RelDirection = RelLeft | RelRight | RelBoth
data RelInfo 
  = OneEdge (Maybe (E Identifier))
  | ManyEdges Range

sho :: (Show a, Monoid s, IsString s) => a -> s
sho = fromString . show

perhaps :: (Monoid s, IsString s) => (a -> s) -> Maybe a -> s
perhaps = maybe ""

writeAssocs :: (Monoid s, IsString s) => [Assoc] -> s
writeAssocs [] = ""
writeAssocs xs = "{ " <> mconcat (intersperse ", " (map f xs))  <> " }"
  where
  f (Assoc name expr) = fromString name <> ": '" <> writeExp expr <> "'"

writeRange :: (Monoid s, IsString s) => Range -> s
writeRange (Range ml mh) = case (ml, mh) of
  (Just l, Just h) ->  sho l <> (if l == h then "" else theDots <> sho h)
  (Just l, Nothing)  -> sho l <> theDots
  (Nothing, Just h)  -> theDots <> sho h
  (Nothing, Nothing) -> ""
  where
  theDots = ".."

writeCases :: (Monoid s, IsString s) => [(E a, E b)] -> Maybe (E b) -> s
writeCases cases def = 
  mconcat ["WHEN " <> writeExp val <> " THEN " <> writeExp result
          | (val, result) <- cases ]
  <> perhaps (\e -> " ELSE " <> writeExp e) def
  <> " END"

writeRelTypes :: (Monoid s, IsString s) => [RelType] -> s
writeRelTypes [] = mempty
writeRelTypes (x : xs) = ":" <> s x <> mconcat [ "|" <> s y | y <- xs ]
  where
  s :: (Monoid s, IsString s) => RelType -> s
  s (RelType y) = fromString y

writeExp :: (Monoid s, IsString s) => E a -> s
writeExp e = case e of
  EInt     i -> sho i
  EDouble  d -> sho d
  EBool    b -> sho b
  EString  s -> sho s
  EIdent   i -> fromString i
  EProp  i p -> writeExp i <> "." <> fromString p

  EColl xs -> sqbrack . mconcat . intersperse "," $
    map writeExp xs
  EIndex xs i -> writeExp xs <> sqbrack (sho i)
  ESubColl xs range -> writeExp xs <> sqbrack (writeRange range)

  EPlus  l r -> binOp "+" l r
  EMinus l r -> binOp "-" l r
  ETimes l r -> binOp "*" l r
  EDiv   l r -> binOp "/" l r
  EMod   l r -> binOp "%" l r
  EPow   l r -> binOp "^" l r

  EAnd l r -> binOp "AND" l r
  EOr  l r -> binOp "OR"  l r
  EXor l r -> binOp "XOR" l r
  ENot x -> "NOT" <> paren (writeExp x)

  ELT  l r -> binOp "<"  l r
  ELTE l r -> binOp "<=" l r
  EGT  l r -> binOp ">"  l r
  EGTE l r -> binOp ">=" l r

  EConcat l r -> binOp "+" l r
  EConcatStr l r -> binOp "+" l r

  ENode ident assocs labels -> paren $
    perhaps writeExp ident 
      <> mconcat [ ":" <> fromString l | Label l <- labels ]
      <> writeAssocs assocs
  ERel left right info dir assocs types -> 
    writeExp left <> leftArr dir <> sqbrack (
      includeInfo info (writeRelTypes types)
      <> writeAssocs assocs
      ) <> rightArr dir <> writeExp right

  ESCase test cases def -> "CASE " <> writeExp test <> " "
    <> writeCases cases def
  EGCase cases def -> "CASE " <> writeCases cases def
  where
  includeInfo :: (Monoid s, IsString s) => RelInfo -> (s -> s)
  includeInfo (OneEdge ident) = (perhaps writeExp ident <>)
  includeInfo (ManyEdges r) = (<> "*" <> writeRange r)
  leftArr RelLeft = "<-"
  leftArr _ = "-"
  rightArr RelRight = "->"
  rightArr _ = "-"
  bracket :: (Monoid s, IsString s) => s -> s -> s -> s
  bracket open close x = open <> x <> close
  paren :: (Monoid s, IsString s) => s -> s
  paren = bracket "(" ")"
  sqbrack :: (Monoid s, IsString s) => s -> s
  sqbrack = bracket "[" "]"
  binOp :: (Monoid s, IsString s) => s -> E a -> E b -> s
  binOp op l r = paren (writeExp l) <> " " <> op 
                 <> " " <> paren (writeExp r)

data Assoc = forall a. Assoc String (E a)

data E :: CType -> * where 
  EInt      :: Int -> E Number
  EDouble   :: Double -> E Number
  EBool     :: Bool -> E Boolean
  EString   :: String -> E Str
  EIdent    :: String -> E Identifier
  EProp     :: E Identifier -> String -> E Property
  EColl     :: [E a] -> E (Collection a)
  EIndex    :: E (Collection a) -> Int -> E a
  ESubColl  :: E (Collection a) -> Range -> E (Collection a)

  ENode  :: Maybe (E Identifier) -> [Assoc] -> [Label] -> E Pattern
  ERel   :: E Pattern -> E Pattern
    -> RelInfo
    -> RelDirection -> [Assoc] -> [RelType] -> E Pattern

  ESCase :: Case a => E a -> [(E a, E b)] -> Maybe (E b) -> E b
  EGCase :: [(E Boolean, E a)] -> Maybe (E a) -> E a
  EPlus, EMinus, ETimes, EDiv, EPow, EMod
    :: E Number -> E Number -> E Number
  EAnd, EOr, EXor :: E Boolean -> E Boolean -> E Boolean
  ENot :: E Boolean -> E Boolean
  EConcatStr :: E Str -> E Str -> E Str
  EConcat :: E (Collection a) -> E (Collection a) -> E (Collection a)
  ELT, ELTE, EGT, EGTE :: EOrd a => E a -> E a -> E Boolean
 
data Query where
  QMatch :: EOrd ord =>
      E Pattern -- ^ match
   -> [E a] -- ^ return
   -> Maybe (E ord) -- ^ order by
   -> Maybe Int -- ^ skip
   -> Maybe Int -- ^ limit
   -> Query
  QUnion :: Bool -> Query -> Query -> Query

writeQuery :: (Monoid s, IsString s) => Query -> s
writeQuery query = case query of
  QMatch match ret orderBy skip limit -> 
    "MATCH " <> writeExp match <> " RETURN " 
    <> mconcat (intersperse ", " (map writeExp ret))
    <> perhaps (\o -> " ORDER BY " <> writeExp o) orderBy
    <> perhaps (\s -> " SKIP " <> sho s) skip
    <> perhaps (\l -> " LIMIT " <> sho l) limit
  QUnion uall left right ->
    writeQuery left <> " UNION " <> (if uall then "ALL " else "")
    <> writeQuery right


simpleMatch :: E Pattern -> [E a] -> Query
simpleMatch match ret = 
  QMatch match ret (Nothing :: Maybe (E Number)) Nothing Nothing

example :: Query
example = simpleMatch (ENode (Just n) [] []) [n] where
  n = EIdent "n"

example2 :: Query
example2 = simpleMatch
  (ERel left right (ManyEdges range) RelBoth [] ["KNOWS"])
  [EProp remote_friend "name"]
  where
  range = Range (Just 1) (Just 2)
  remote_friend = EIdent "remote_friend"
  me = EIdent "me"
  left = ENode (Just me) [] []
  right = ENode (Just remote_friend) [] []
