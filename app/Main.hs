{-# LANGUAGE OverloadedStrings #-}

module Main where

import Person

import qualified Control.Monad as Monad
import qualified System.Exit as Exit

main :: IO ()
main = do
  putStrLn "Opening data..."

  eitherPersons <- decodePersonsFromFile "data/data.csv"

  case eitherPersons of
    Left reason -> Exit.die reason

    Right persons -> do
      putStrLn $ "Number of persons: " ++ show (length persons)

      putStrLn "Printing persons to data/out.csv..."

      Monad.void (encodePersonsToFile "data/out.csv" persons)

      putStrLn "All done! Goodbye! :-)"

    {-csvData <- BL.readFile "data/data.csv"
    case decodeByName csvData of
        Left err -> putStrLn err
        Right (_, v) -> V.forM_ v $ \ p ->
            putStrLn $ (unlines $ names p) ++ " is from " ++ show (address p) ++ "!"-}