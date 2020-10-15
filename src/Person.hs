{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Person where

import Control.Exception (IOException)
import qualified Control.Exception as Exception
import qualified Data.Foldable as Foldable

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL

import Data.Csv (
    DefaultOrdered(headerOrder)
  , FromField(parseField)
  , FromNamedRecord(parseNamedRecord)
  , Header
  , ToField(toField)
  , ToNamedRecord(toNamedRecord)
  , (.:)
  , (.=)
  )
import qualified Data.Csv as Cassava

import Data.Text (Text)
import qualified Data.Text.Encoding as Text

import Data.Vector (Vector)
import qualified Data.Vector as Vector

data Person = Person {
    name :: Text
  , gender :: Text
  , age :: Int
  , dob :: Text
  , telNo :: Int
  , email :: Text
} deriving (Eq, Show)

testPersonHeader :: ByteString
testPersonHeader = "name,gender,age,dob,telNo,email"

testPersonRecord :: ByteString
testPersonRecord = "John,Male,35,1985-6-20,1234567890,john@abc.net"

testPersonData :: ByteString
testPersonData = testPersonHeader <> "\n" <> testPersonRecord

testPersonItem :: Person
testPersonItem = Person {
    name = "John"
  , gender = "Male"
  , age = 35
  , dob = "1985-6-20"
  , telNo = 1234567890
  , email = "john@abc.net"
}

instance FromNamedRecord Person where
  parseNamedRecord m =
    Person 
      <$> fmap Text.decodeLatin1 (m .: "name")
      <*> m .: "gender"
      <*> m .: "age"
      <*> m .: "dob"
      <*> m .: "telNo"
      <*> m .: "email"


instance ToNamedRecord Person where
  toNamedRecord Person{..} =
    Cassava.namedRecord
      [ "name" .= name
      , "age" .= age
      , "dob" .= dob
      , "telNo" .= telNo
      , "email" .= email
      ]

instance DefaultOrdered Person where
  headerOrder _ = Cassava.header ["name", "age", "dob", "telNo", "email"]

personHeader :: Header
personHeader = Vector.fromList ["name", "age", "dob", "telNo", "email"]
    

type DecodeResult = Either String (Vector Person)

decodePersons :: ByteString -> DecodeResult
decodePersons = fmap snd . Cassava.decodeByName

decodePersonsFromFile :: FilePath -> IO DecodeResult
decodePersonsFromFile filePath = 
  catchShowIO (BL.readFile filePath) 
  >>= return . either Left decodePersons

type EncodeResult = Either String ()

encodePersons :: Vector Person -> ByteString
encodePersons = Cassava.encodeDefaultOrderedByName . Foldable.toList

encodePersonsToFile :: FilePath -> Vector Person -> IO EncodeResult
encodePersonsToFile filePath = catchShowIO . BL.writeFile filePath . encodePersons

catchShowIO :: IO a -> IO (Either String a)
catchShowIO action =
  fmap Right action `Exception.catch` handleIOException
  where
    handleIOException :: IOException -> IO (Either String a)
    handleIOException = return . Left . show
{-
import Time

import Data.Default

data Address = Address {
        streetAddress :: String
    ,   city :: String
    ,   province :: String
    ,   postalCode :: String
} deriving (Eq, Read, Show)

instance Default Address where
    def = Address "" "" "" ""

data Relation = Family | Friend | Colleague | Unknown deriving (Eq, Read, Show)

instance Default Relation where
    def = Unknown

data Person = Person {
        names    :: [String] -- Unlimited names!
    ,   gender   :: String   -- You can use any gender you like :)
    ,   age      :: Int
    ,   dob      :: GregorianDate
    ,   address  :: Address
    ,   telNo    :: String
    ,   email    :: String
    ,   relation :: Relation
} deriving (Read, Show)

instance Default Person where
    def = Person [""] "" 0 (def :: GregorianDate) (def :: Address) "" "" Unknown

queryByLastName :: [Person] -> String -> [Person]
queryByLastName pool target = filter (\p -> (last $ names p) == target) pool

queryByAge :: [Person] -> Int -> [Person]
queryByAge pool target = filter (\p -> (age p) == target) pool

queryByDOB :: [Person] -> GregorianDate -> [Person]
queryByDOB pool target = filter (\p -> (dob p) == target) pool

-- queryByDOBinRange :: 

queryByRelation :: [Person] -> Relation -> [Person]
queryByRelation pool target = filter (\p -> (relation p) == target) pool
-}