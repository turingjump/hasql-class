module Hasql.Class.Internal.Encodable where

import Data.Default.Class (def)
import qualified Hasql.Encoders as Hasql
import Data.Time (DiffTime, UTCTime, Day, TimeOfDay, LocalTime)
import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.Functor.Contravariant.Generic
import Data.Int (Int16, Int32, Int64)
import Data.Proxy (Proxy(..))

-- | Datatypes that can be encoded as `hasql` PostgreSQL parameters. This class
-- can be generically derived.
--
-- Note that the number of parameters is not necessarily the number of Haskell
-- values. For example
--
-- > data MyData = MyData { aChar :: Char, aText :: Text }
-- >  deriving (Eq, Show, Generic, Encodable)
-- >
-- > aData :: MyData
-- > aData = MyDate 'a' "ha!"
-- >
-- > -- Will only insert the char, and a NULL for the text value
-- > wrong = query aData stmtUnit "INSERT INTO myTable ($1)" True
-- >
-- > -- Will insert both the char and the text values
-- > right = query aData stmtUnit "INSERT INTO myTable ($1, $2)" True
--
-- #SINCE#
class Encodable a where
  encode :: Hasql.Params a
  default encode :: Deciding Encodable a => Hasql.Params a
  encode = deciding (Proxy :: Proxy Encodable) encode

-- | #SINCE#
instance Encodable () where
  encode = Hasql.unit
-- | #SINCE#
instance Encodable Char where
  encode = Hasql.value def
-- | #SINCE#
instance Encodable Bool where
  encode = Hasql.value def
-- | #SINCE#
instance Encodable Int16 where
  encode = Hasql.value def
-- | #SINCE#
instance Encodable Int32 where
  encode = Hasql.value def
-- | #SINCE#
instance Encodable Int64 where
  encode = Hasql.value def
-- | #SINCE#
instance Encodable Double where
  encode = Hasql.value def
-- | #SINCE#
instance Encodable Float where
  encode = Hasql.value def
-- | #SINCE#
instance Encodable Text where
  encode = Hasql.value def
-- | #SINCE#
instance Encodable ByteString where
  encode = Hasql.value def
-- | #SINCE#
instance Encodable DiffTime where
  encode = Hasql.value def
-- | #SINCE#
instance Encodable UTCTime where
  encode = Hasql.value def
-- | #SINCE#
instance Encodable Day where
  encode = Hasql.value def
-- | #SINCE#
instance Encodable TimeOfDay where
  encode = Hasql.value def
-- | #SINCE#
instance Encodable LocalTime where
  encode = Hasql.value def
