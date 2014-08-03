{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Database.Neo4j.Types
  ( 
    Server(..)
  , DValue(..)
  ) where

import Control.Applicative ((<$>))

import Data.Text (Text)
import Data.Data (Data)
import Data.Int (Int64)
import Data.Typeable (Typeable)

import qualified Data.Vector as V

import qualified Data.Aeson as A
import Data.Scientific (toRealFloat, coefficient, base10Exponent)

data DValue
  = Int !Int64
  | Float !Double
  | String !Text
  | Bool !Bool
  | DColl !(V.Vector DValue)
  | DObj !A.Object
  | Null
  deriving (Eq, Show, Data, Typeable)

instance A.FromJSON DValue where
  parseJSON (A.Object o) = return $ DObj o
  parseJSON (A.Array a) = DColl <$> V.mapM A.parseJSON a
  parseJSON (A.String xs) = return $ String xs
  parseJSON (A.Bool b) = return $ Bool b
  parseJSON A.Null = return Null
  parseJSON (A.Number n) = return $! numberToValue
    where
      numberToValue
        -- If the number is larger than Int64, it must be
        -- a float64 (Double in Haskell).
        | n > maxInt = Float $ toRealFloat n
        | e < 0 = Float $ realToFrac n
        | otherwise = Int $ fromIntegral $ coefficient n * 10 ^ e
        where
          e = base10Exponent n
      maxInt = fromIntegral (maxBound :: Int64)

instance A.ToJSON DValue where
  toJSON (Int n) = A.toJSON n
  toJSON (Float d) = A.toJSON d
  toJSON (String xs) = A.toJSON xs
  toJSON (Bool b) = A.toJSON b
  toJSON (DObj o) = A.Object o
  toJSON (DColl xs) = A.Array (V.map A.toJSON xs)
  toJSON Null = A.Null


-- | Server location.
data Server = Server
  { serverURI :: !Text
  -- ^ full URI to server API (db/data/) -- http://host:port/db/data/
  } deriving Show
