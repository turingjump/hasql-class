module Hasql.Class
  ( Encodable(..)
  , Decodable(..)
  , stmtList
  , stmtUnit
  ) where

import Hasql.Class.Encodable
import Hasql.Class.Decodable

import Data.ByteString (ByteString)
import Hasql.Query
import Hasql.Decoders (rowsList, unit)

stmtList :: (Encodable a, Decodable b) => ByteString -> Bool -> Query a [b]
stmtList query isPrepared = statement query encode (rowsList decode) isPrepared

stmtUnit :: (Encodable a) => ByteString -> Bool -> Query a ()
stmtUnit query isPrepared = statement query encode unit isPrepared
