{-# LANGUAGE GADTs, DataKinds, KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving #-}
{-# LANGUAGE TypeOperators, TypeFamilies, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Language.Cypher where


import qualified Data.Aeson as A
import Data.List (intersperse)
import Data.Monoid (Monoid (..), (<>))
import Data.String (IsString (fromString))
import Data.Text (Text)

import qualified Data.Vector as V

import Database.Neo4j.Types (DValue (..))

data CType = 
    Number | Boolean | Str
  | Identifier
  | Collection CType
  | CPattern

class Convert (a :: CType) where
  data Value a :: * 
  convert :: DValue -> Maybe (Value a)

instance Convert Number where
  newtype Value Number = VNum Double deriving (Eq, Show, Ord)
  convert (Int x) = Just $ VNum (fromIntegral x)
  convert (Float x) = Just $ VNum x
  convert _ = Nothing

instance Convert Str where
  newtype Value Str = VStr Text deriving (Eq, Show, Ord)
  convert (String x) = Just $ VStr x
  convert _ = Nothing

instance Convert Boolean where
  newtype Value Boolean = VBool Bool deriving (Eq, Show, Ord)
  convert (Bool x) = Just $ VBool x
  convert _ = Nothing

instance Convert Identifier where
  newtype Value Identifier = VIdent A.Object deriving (Eq, Show)
  convert (DObj o) = Just $ VIdent o
  convert _ = Nothing

instance Convert a => Convert (Collection a) where
  newtype Value (Collection a) = VColl (V.Vector (Value a))
  convert (DColl xs) = fmap VColl $ V.mapM convert xs
  convert _ = Nothing

instance Show (Value a) => Show (Value (Collection a)) where
  show (VColl xs) = "VColl " ++ show xs

class Case (a :: CType)
instance Case Number
instance Case Str
instance Case Boolean

class EOrd (a :: CType)
instance EOrd Number

newtype Label = Label String deriving (IsString)
newtype RelType = RelType String deriving (IsString)

data Range = Range (Maybe Int) (Maybe Int)

data RelDirection = RelLeft | RelRight | RelBoth
--data RelInfo = RelInfo (Maybe (E Identifier)) (Maybe Range)

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
  PRel left right ident mr dir assocs types -> 
    writePattern left <> leftArr dir <> sqbrack (
      (perhaps writeExp ident <> writeRelTypes types <> perhaps (\r -> "*" <> writeRange r) mr)
      <> writeAssocs assocs
      ) <> rightArr dir <> writePattern right
  PAnd left right -> writePattern left <> " , " <> writePattern right
  where
  leftArr RelLeft = "<-"
  leftArr _ = "-"
  rightArr RelRight = "->"
  rightArr _ = "-"
    

writeExp :: (Monoid s, IsString s) => E a -> s
writeExp e = case e of
  EInt     i -> sho i
  EDouble  d -> sho d
  EBool    b -> sho b
  EString  s -> sho s
  EIdent   i -> fromString i
  EParam   p -> "{" <> fromString p <> "}"
  EProp  i p -> writeExp i <> "." <> fromString p

  EAbs i -> "abs(" <> writeExp i <> ")"
  ESign i -> "sign(" <> writeExp i <> ")"

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

  EEQ l r -> binOp "=" l r
  ERegExpEQ l r -> binOp "=~" l r

  EConcat l r -> binOp "+" l r
  EConcatStr l r -> binOp "+" l r


  ESCase test cases def -> "CASE " <> writeExp test <> " "
    <> writeCases cases def
  EGCase cases def -> "CASE " <> writeCases cases def

  EPattern pat -> writePattern pat
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

instance Num (E Number) where
  x + y = EPlus x y
  x * y = ETimes x y
  x - y = EMinus x y
  negate x = EMinus (EDouble 0) x
  fromInteger x = EInt (fromIntegral x)
  abs x = EAbs x 
  signum x = ESign x

data E :: CType -> * where
  -- literals
  EInt      :: Int -> E Number
  EDouble   :: Double -> E Number
  EBool     :: Bool -> E Boolean
  EString   :: String -> E Str
  EIdent    :: String -> E Identifier
  EParam    :: String -> E a

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

  --functions
  EAbs, ESign :: E Number -> E Number

  EEQ :: EEq a => E a -> E a -> E Boolean
  ERegExpEQ :: E Str -> E Str -> E Boolean

  EPattern :: Pattern -> E CPattern

class EEq (a :: CType) where

instance EEq Str
instance EEq Number
instance EEq Boolean

data EAs :: CType -> * where
  EAs :: E a -> String -> EAs a

data RetE :: CType -> * where
  RetE :: E a -> RetE a
  RetEAs :: EAs a -> RetE a

writeAs :: (IsString s, Monoid s) => EAs a -> s
writeAs (EAs expr name) = writeExp expr <> " AS " <> fromString name

writeRetE :: (IsString s, Monoid s) => RetE a -> s
writeRetE x = case x of
  RetE e -> writeExp e
  RetEAs e -> writeAs e

data Pattern =
    PNode (Maybe (E Identifier)) [Assoc] [Label]
  | PRel Pattern Pattern (Maybe (E Identifier)) (Maybe Range) RelDirection [Assoc] [RelType]
  | PAnd Pattern Pattern

data MatchType = RequiredMatch | OptionalMatch
data Match = Match MatchType Pattern (Maybe Where)

writeMatchType :: (IsString s, Monoid s) => MatchType -> s
writeMatchType x = case x of
  RequiredMatch -> "MATCH"
  OptionalMatch -> "OPTIONAL MATCH"

writeMatch :: (IsString s, Monoid s) => Match -> s
writeMatch (Match mt pat mwhere) = writeMatchType mt
  <> " " <> writePattern pat <> perhaps (\w -> " " <> writeWhere w) mwhere

class WhereExp (e :: CType)
instance WhereExp Boolean
instance WhereExp CPattern

data Where where
  Where :: WhereExp e => E e -> Where

writeWhere :: (IsString s, Monoid s) => Where -> s
writeWhere (Where expr) = "WHERE " <> writeExp expr

 
data Query (l :: [CType]) where
  QReturn :: 
      [Match] -- ^ matches
   -> HList RetE xs -- ^ return
   -> Maybe (E ord) -- ^ order by
   -> Maybe Int -- ^ skip
   -> Maybe Int -- ^ limit
   -> Query xs
  QUnion :: Bool -> Query xs -> Query xs -> Query xs
  QWith :: EAs a -> Query xs -> Query xs

infixr 5 :::
data HList f (as :: [CType]) where
  HNil :: HList f '[]
  (:::) :: f a -> HList f as -> HList f (a ': as)

instance Show (HList f '[]) where
  show HNil = "HNil"

instance Eq (HList f '[]) where
  HNil == HNil = True

instance (Show (f a), Show (HList f as)) => Show (HList f (a ': as)) where
  show (x ::: xs) = show x ++ " ::: " ++ show xs

instance (Eq (f a), Eq (HList f as)) => Eq (HList f (a ': as)) where
  (x ::: xs) == (y ::: ys) = x == y && xs == ys

foldrHList :: (forall a. RetE a -> b -> b) -> b -> HList RetE xs -> b
foldrHList _ z HNil = z
foldrHList f z (x ::: xs) = f x (foldrHList f z xs)

mapHList :: (forall a. RetE a -> b) -> HList RetE xs -> [b]
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
  [ [Float 3 , Bool True , String "Hi!", DColl (V.fromList [Float 3, Float 4])]
  , [Float 15, Bool False, String "HaskellDC", DColl V.empty]
  ]
-- an example of converting to our heterogenous list
typedResult :: [HList Value [Number, Boolean, Str, Collection Number]]
Just typedResult = mapM convertl dresult

writeQuery :: (Monoid s, IsString s) => Query xs -> s
writeQuery query = case query of
  QReturn matches ret orderBy skip limit -> 
    mconcat (map writeMatch matches)
    <> " RETURN " <> mconcat (intersperse ", " (mapHList writeRetE ret))
    <> perhaps (\o -> " ORDER BY " <> writeExp o) orderBy
    <> perhaps (\s -> " SKIP " <> sho s) skip
    <> perhaps (\l -> " LIMIT " <> sho l) limit
  QUnion uall left right ->
    writeQuery left <> " UNION " <> (if uall then "ALL " else "")
    <> writeQuery right
  QWith asExpr q ->
    "WITH " <> writeAs asExpr <> writeQuery q

simpleMatch :: Pattern -> HList RetE xs -> Query xs
simpleMatch matchPat ret = 
  QReturn [Match RequiredMatch matchPat Nothing]
    ret (Nothing :: Maybe (E Number)) Nothing Nothing

