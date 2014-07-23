{-# LANGUAGE GADTs, DataKinds, KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving #-}
{-# LANGUAGE TypeOperators, TypeFamilies, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Language.Cypher where

import Data.List (intersperse)
import Data.Monoid (Monoid (..), (<>))
import Data.String (IsString (fromString))

data CType = 
    Number | Boolean | Str
  | Identifier
  | Collection CType

data DValue = DNum Double | DBool Bool | DStr String | DColl [DValue]
  deriving (Eq, Show)

class Convert (a :: CType) where
  data Value a :: * 
  convert :: DValue -> Maybe (Value a)

instance Convert Number where
  data Value Number = VNum Double deriving Show
  convert (DNum x) = Just $ VNum x
  convert _ = Nothing

instance Convert Str where
  data Value Str = VStr String deriving Show
  convert (DStr x) = Just $ VStr x
  convert _ = Nothing

instance Convert Boolean where
  data Value Boolean = VBool Bool deriving Show
  convert (DBool x) = Just $ VBool x
  convert _ = Nothing

instance Convert a => Convert (Collection a) where
  data Value (Collection a) = VColl [Value a]
  convert (DColl xs) = fmap VColl $ mapM convert xs
  convert _ = Nothing

instance Show (Value a) => Show (Value (Collection a)) where
  show (VColl xs) = "VColl " ++ show xs

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

writePattern :: (Monoid s, IsString s) => Pattern -> s
writePattern p = case p of
  PNode ident assocs labels -> paren $
    perhaps writeExp ident 
      <> mconcat [ ":" <> fromString l | Label l <- labels ]
      <> writeAssocs assocs
  PRel left right info dir assocs types -> 
    writePattern left <> leftArr dir <> sqbrack (
      includeInfo info (writeRelTypes types)
      <> writeAssocs assocs
      ) <> rightArr dir <> writePattern right
  PAnd left right -> writePattern left <> " , " <> writePattern right
  where
  leftArr RelLeft = "<-"
  leftArr _ = "-"
  rightArr RelRight = "->"
  rightArr _ = "-"
  includeInfo :: (Monoid s, IsString s) => RelInfo -> (s -> s)
  includeInfo (OneEdge ident) = (perhaps writeExp ident <>)
  includeInfo (ManyEdges r) = (<> "*" <> writeRange r)

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


  ESCase test cases def -> "CASE " <> writeExp test <> " "
    <> writeCases cases def
  EGCase cases def -> "CASE " <> writeCases cases def
  where
  binOp :: (Monoid s, IsString s) => s -> E a -> E b -> s
  binOp op l r = paren (writeExp l) <> " " <> op 
                 <> " " <> paren (writeExp r)

bracket :: (Monoid s, IsString s) => s -> s -> s -> s
bracket open close x = open <> x <> close
sqbrack :: (Monoid s, IsString s) => s -> s
sqbrack = bracket "[" "]"
paren :: (Monoid s, IsString s) => s -> s
paren = bracket "(" ")"

data Assoc :: * where
  Assoc :: String -> E a -> Assoc

data E :: CType -> * where
  -- literals
  EInt      :: Int -> E Number
  EDouble   :: Double -> E Number
  EBool     :: Bool -> E Boolean
  EString   :: String -> E Str
  EIdent    :: String -> E Identifier

  EProp     :: E Identifier -> String -> E a
  EColl     :: [E a] -> E (Collection a)
  EIndex    :: E (Collection a) -> Int -> E a
  ESubColl  :: E (Collection a) -> Range -> E (Collection a)

  ESCase :: Case a => E a -> [(E a, E b)] -> Maybe (E b) -> E b
  EGCase :: [(E Boolean, E a)] -> Maybe (E a) -> E a

  --operators
  EPlus, EMinus, ETimes, EDiv, EPow, EMod
    :: E Number -> E Number -> E Number
  EAnd, EOr, EXor :: E Boolean -> E Boolean -> E Boolean
  ENot :: E Boolean -> E Boolean
  EConcatStr :: E Str -> E Str -> E Str
  EConcat :: E (Collection a) -> E (Collection a) -> E (Collection a)
  ELT, ELTE, EGT, EGTE :: EOrd a => E a -> E a -> E Boolean

data Pattern =
    PNode (Maybe (E Identifier)) [Assoc] [Label]
  | PRel Pattern Pattern RelInfo RelDirection [Assoc] [RelType]
  | PAnd Pattern Pattern
 
data Query (l :: [CType]) where
  QMatch :: EOrd ord =>
      Pattern -- ^ match
   -> HList E xs -- ^ return
   -> Maybe (E ord) -- ^ order by
   -> Maybe Int -- ^ skip
   -> Maybe Int -- ^ limit
   -> Query xs
  QUnion :: Bool -> Query xs -> Query xs -> Query xs

data HList f (as :: [CType]) where
  HNil :: HList f '[]
  (:::) :: f a -> HList f as -> HList f (a ': as)

instance Show (HList f '[]) where
  show HNil = "HNil"

instance (Show (f a), Show (HList f as)) => Show (HList f (a ': as)) where
  show (x ::: xs) = show x ++ " ::: " ++ show xs

foldrHList :: (forall a. E a -> b -> b) -> b -> HList E xs -> b
foldrHList _ z HNil = z
foldrHList f z (x ::: xs) = f x (foldrHList f z xs)

mapHList :: (forall a. E a -> b) -> HList E xs -> [b]
mapHList f = foldrHList ((:) . f) []

instance Show (Query xs) where
  show = writeQuery :: Query xs -> String

class ConvertL (as :: [CType]) where
  convertl :: [DValue] -> Maybe (HList Value as)

instance ConvertL '[] where
  convertl [] = Just HNil
  convertl _ = Nothing

instance (Convert a, ConvertL as) => ConvertL (a ': as) where
  convertl (y : ys) = do
    y' <- convert y 
    ys' <- convertl ys
    return $ y' ::: ys'
  convertl _ = Nothing

dresult :: [[DValue]]
dresult = 
  [ [DNum 3 , DBool True , DStr "Hi!", DColl [DNum 3, DNum 4]]
  , [DNum 15, DBool False, DStr "HaskellDC", DColl []]
  ]
-- an example of converting to our heterogenous list
typedResult :: [HList Value [Number, Boolean, Str, Collection Number]]
Just typedResult = mapM convertl dresult

writeQuery :: (Monoid s, IsString s) => Query xs -> s
writeQuery query = case query of
  QMatch match ret orderBy skip limit -> 
    "MATCH " <> writePattern match <> " RETURN " 
    <> mconcat (intersperse ", " (mapHList writeExp ret))
    <> perhaps (\o -> " ORDER BY " <> writeExp o) orderBy
    <> perhaps (\s -> " SKIP " <> sho s) skip
    <> perhaps (\l -> " LIMIT " <> sho l) limit
  QUnion uall left right ->
    writeQuery left <> " UNION " <> (if uall then "ALL " else "")
    <> writeQuery right

simpleMatch :: Pattern -> HList E xs -> Query xs
simpleMatch match ret = 
  QMatch match ret (Nothing :: Maybe (E Number)) Nothing Nothing

