{-# LANGUAGE OverloadedStrings #-}

module Main where

import Input
import Lib
import Person
import Time

import Control.Applicative
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V

instance FromNamedRecord Address where
    parseNamedRecord r = Address <$>
        r .: "streetAddress" <*>
        r .: "city" <*>
        r .: "province" <*>
        r .: "postalCode"

instance FromNamedRecord GregorianDate where
    parseNamedRecord r = GregorianDate <$>
        r .: "year"  <*>
        r .: "month" <*>
        r .: "day"

instance FromNamedRecord Person where
    parseNamedRecord r = Person <$> 
        r .: "name"          <*> 
        r .: "gender"        <*> 
        r .: "age"           <*> 
        r .: "DOB"           <*> 
        r .: "address"       <*>
        r .: "telNo"         <*>
        r .: "email"         <*>
        r .: "relation"

main :: IO ()
main = do
    csvData <- BL.readFile "data/data.csv"
    case decodeByName csvData of
        Left err -> putStrLn err
        Right (_, v) -> V.forM_ v $ \ p ->
            putStrLn $ name p ++ " is from " ++ show (province p) ++ "!"