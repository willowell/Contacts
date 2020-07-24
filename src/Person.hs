module Person where

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