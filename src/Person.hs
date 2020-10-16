{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


module Person where


import Control.Exception (IOException)
import qualified Control.Exception as Exception
import qualified Data.Foldable as Foldable


import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL

import qualified Data.Either as Either


import Data.Csv (Parser, 
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


import Data.Text (unwords, splitOn, Text)
import qualified Data.Text.Encoding as Text


import Data.Vector (Vector)
import qualified Data.Vector as Vector


data Person = Person {
    names  :: [Text]
  , gender :: Text
  , age    :: Int
  , dob    :: Text
  , telNo  :: Int
  , email  :: Text
  , relation :: Relation
} deriving (Eq, Show)


data Address 
  = Address {
      streetAddress :: Text
    , city          :: Text
    , province      :: Text
    , postalCode    :: Text
  } 
  | UnknownAddress Text 
  deriving (Eq, Show)


data Relation 
  = Family 
  | Friend 
  | Colleague
  | Business
  | UnknownRelation Text
  deriving (Eq, Show)


instance FromNamedRecord Person where
  parseNamedRecord m =
    Person <$> 
      parseNames <$> (m .: "names")
      <*> m .: "gender"
      <*> m .: "age"
      <*> m .: "dob"
      <*> m .: "telNo"
      <*> m .: "email"
      <*> m .: "relation"

parseNames = Data.Text.splitOn " " . Text.decodeLatin1

instance ToNamedRecord Person where
  toNamedRecord Person{..} =
    Cassava.namedRecord
      [ "names" .= Data.Text.unwords names
      , "gender" .= gender
      , "age" .= age
      , "dob" .= dob
      , "telNo" .= telNo
      , "email" .= email
      , "relation" .= relation
      ]


instance FromField Relation where
  parseField "family" = pure Family

  parseField "friend" = pure Friend

  parseField "colleague" = pure Colleague

  parseField "business" = pure Business

  parseField unknown = UnknownRelation <$> parseField unknown


instance ToField Relation where
  toField Family = "family"

  toField Friend = "friend"

  toField Colleague = "colleague"

  toField Business = "business"

  toField (UnknownRelation unknown) = toField unknown


instance DefaultOrdered Person where
  headerOrder _ = Cassava.header ["names", "gender", "age", "dob", "telNo", "email", "relation"]


personHeader :: Header
personHeader = Vector.fromList ["names", "gender", "age", "dob", "telNo", "email", "relation"]
    
{--------------------------------------------------------------------------------------------------}
{-                                 ENCODERS AND DECODERS                                          -}
{--------------------------------------------------------------------------------------------------}

type DecodeResult = Either String (Vector Person)
type EncodeResult = Either String ()


decodePersons :: ByteString -> DecodeResult
decodePersons = fmap snd . Cassava.decodeByName


encodePersons :: Vector Person -> ByteString
encodePersons = Cassava.encodeDefaultOrderedByName . Foldable.toList


decodePersonsFromFile :: FilePath -> IO DecodeResult
decodePersonsFromFile filePath = 
  catchShowIO (BL.readFile filePath) 
  >>= return . either Left decodePersons


encodePersonsToFile :: FilePath -> Vector Person -> IO EncodeResult
encodePersonsToFile filePath = catchShowIO . BL.writeFile filePath . encodePersons


catchShowIO :: IO a -> IO (Either String a)
catchShowIO action =
  fmap Right action `Exception.catch` handleIOException
  where
    handleIOException :: IOException -> IO (Either String a)
    handleIOException = return . Left . show

{--------------------------------------------------------------------------------------------------}
{-                                            QUERIES                                             -}
{--------------------------------------------------------------------------------------------------}

query :: Vector Person -> (Person -> Bool) -> Vector Person
query pool by = Vector.filter by pool


byFirstName :: Text -> (Person -> Bool)
byFirstName target = (\p -> (head $ names p) == target)



byLastName :: Text -> (Person -> Bool)
byLastName target = (\p -> (last $ names p) == target)


byGender :: Text -> (Person -> Bool)
byGender target = (\p -> (gender p) == target)


byAge :: Int -> (Person -> Bool)
byAge target = (\p -> (age p) == target)


byRelation :: Relation -> (Person -> Bool)
byRelation target = (\p -> (relation p) == target)

{--------------------------------------------------------------------------------------------------}
{-                                            TESTING                                             -}
{--------------------------------------------------------------------------------------------------}

testPersonHeader :: ByteString
testPersonHeader = "names,gender,age,dob,telNo,email,relation"


testPersonRecord :: ByteString
testPersonRecord = "John Johnson Jobberson Josephstinson Jorgengengensonian,Male,35,1985-6-20,1234567890,john@abc.net,friend"


testPersonData :: ByteString
testPersonData = testPersonHeader <> "\r\n" <> testPersonRecord <> "\r\n"


testPersonItem :: Vector Person
testPersonItem = Vector.fromList 
  [
    Person {
      names = ["John","Johnson","Jobberson","Josephstinson","Jorgengengensonian"]
    , gender = "Male"
    , age = 35
    , dob = "1985-6-20"
    , telNo = 1234567890
    , email = "john@abc.net"
    , relation = Friend
    }
 ]

testDecode :: Bool
testDecode = 
  (== testPersonItem) 
  $ Either.fromRight undefined 
  $ decodePersons testPersonData

testEncode :: Bool
testEncode =
  (== testPersonData)
  $ encodePersons testPersonItem

testDecodeAndEncode :: Bool
testDecodeAndEncode = 
  (== testPersonData) 
  $ encodePersons 
  $ Either.fromRight undefined 
  $ decodePersons testPersonData