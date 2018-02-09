{-# LANGUAGE MonoLocalBinds #-}
module Hasql.Class.Internal.Decodable where

import Data.Default.Class (def)
import qualified Hasql.Decoders as Hasql
import Data.Time
import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.Int
import Generics.Eot

-- | Datatypes that can be encoded as `hasql` PostgreSQL parameters.  This
-- class can be generically derived.
--
--   #SINCE#
class Decodable a where
  decode :: Hasql.Row a
  default decode :: (HasEot a, GDecodable (Eot a)) => Hasql.Row a
  decode = fromEot <$> gdecode

-- | #SINCE#
instance Decodable Char where
  decode = Hasql.value def
-- | #SINCE#
instance Decodable Bool where
  decode = Hasql.value def
-- | #SINCE#
instance Decodable Int16 where
  decode = Hasql.value def
-- | #SINCE#
instance Decodable Int32 where
  decode = Hasql.value def
-- | #SINCE#
instance Decodable Int64 where
  decode = Hasql.value def
-- | #SINCE#
instance Decodable Double where
  decode = Hasql.value def
-- | #SINCE#
instance Decodable Float where
  decode = Hasql.value def
-- | #SINCE#
instance Decodable Text where
  decode = Hasql.value def
-- | #SINCE#
instance Decodable ByteString where
  decode = Hasql.value def
-- | #SINCE#
instance Decodable DiffTime where
  decode = Hasql.value def
-- | #SINCE#
instance Decodable UTCTime where
  decode = Hasql.value def
-- | #SINCE#
instance Decodable Day where
  decode = Hasql.value def
-- | #SINCE#
instance Decodable TimeOfDay where
  decode = Hasql.value def
-- | #SINCE#
instance Decodable LocalTime where
  decode = Hasql.value def

class GDecodable eot where
  gdecode :: Hasql.Row eot

instance (Decodable x, GDecodable xs)
  => GDecodable (x, xs) where
  gdecode = (,) <$> decode <*> gdecode

instance GDecodable () where
  gdecode = return ()

instance GDecodable x => GDecodable (Either x Void) where
  gdecode = Left <$> gdecode
