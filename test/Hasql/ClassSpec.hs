{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
module Hasql.ClassSpec (spec) where

import Data.String.QQ (s)
import Data.Int
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Hasql.Connection (acquire, release)
import Hasql.Session (query, Session, Error, sql, run)
import System.Process (callCommand)
import Test.Hspec (describe, it, Spec, shouldBe)
import Test.QuickCheck (Arbitrary(..), property, (==>))
import Test.QuickCheck.Instances ()

import Hasql.Class

spec :: Spec
spec = describe "Encodable" $ do

  it "works for the generic instance"
    $ property $ \(test :: Test) -> noNullBytes test ==> do
      result <- withDB $ do
        () <- query test $ stmtUnit [s|
          INSERT INTO hasql_class_test_table
          VALUES ($1, $2, $3, $4) |] True
        query () $ stmtList "SELECT * FROM hasql_class_test_table" True
      result `shouldBe` Right [test]


-- postgres drops null bytes
noNullBytes :: Test -> Bool
noNullBytes t
  = not (Text.any (== '\NUL') $ t_text t)
  && not (t_char t == '\NUL')


data Test = Test
   { t_int  :: Int16
   , t_text :: Text
   , t_char :: Char
   , t_bool :: Bool
   } deriving (Eq, Show, Read, Generic, Encodable, Decodable)

instance Arbitrary Test where
  arbitrary = Test <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

withDB :: Session b -> IO (Either Error b)
withDB sess = do
   callCommand createCmd
   conn <- acquire info >>= \i -> case i of
     Left err -> error $ show err
     Right v -> return v
   result <- flip run conn $ do
     sql createTbl
     s' <- sess
     sql deleteTbl
     return s'
   release conn
   return result
  where
    info = "dbname=hasql-class-test"
    createCmd = "createdb hasql-class-test >/dev/null 2>/dev/null || true"
    createTbl = [s|
      CREATE TABLE IF NOT EXISTS hasql_class_test_table (
          t_int  integer NOT NULL,
          t_text text    NOT NULL,
          t_char char    NOT NULL,
          t_bool bool    NOT NULL
      )
      |]
    deleteTbl = "DROP TABLE hasql_class_test_table "
