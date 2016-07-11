module Hasql.Class
  (
  -- * Query helpers
  --
  -- | Utility functions for `Query`s. The @ByteString@ argument is the SQL, and
  -- the @Bool@ argument is whether to use prepared statements.
    stmtList
  , stmtUnit
  , stmtVector
  , stmtMaybe
  -- * Classes
  ,  Encodable(..)
  , Decodable(..)
  ) where

import Hasql.Class.Internal.Encodable
import Hasql.Class.Internal.Decodable

import Data.Vector (Vector)
import Data.ByteString (ByteString)
import Hasql.Query
import Hasql.Decoders (rowsList, unit, maybeRow, rowsVector)

-- | Make a `Query` that returns a `Vector` of values
--
-- Faster than `stmtList`.
stmtVector :: (Encodable a, Decodable b) => ByteString -> Bool -> Query a (Vector b)
stmtVector query isPrepared = statement query encode (rowsVector decode) isPrepared

-- | Make a `Query` that @Maybe@ returns a value
stmtMaybe :: (Encodable a, Decodable b) => ByteString -> Bool -> Query a (Maybe b)
stmtMaybe query isPrepared = statement query encode (maybeRow decode) isPrepared

-- | Make a `Query` that returns a list of values
--
-- > getPeople :: Query () Person
-- > getPeople = stmtList "SELECT * FROM person" True
stmtList :: (Encodable a, Decodable b) => ByteString -> Bool -> Query a [b]
stmtList query isPrepared = statement query encode (rowsList decode) isPrepared

-- | Make a `Query` that returns @()@ (no result).
--
-- > insertVal :: Query Text ()
-- > insertVal = stmtUnit "INSERT INTO tbl VALUES ($1)" True
stmtUnit :: (Encodable a) => ByteString -> Bool -> Query a ()
stmtUnit query isPrepared = statement query encode unit isPrepared
