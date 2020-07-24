module Input where

import Data.Char
import System.IO
import Text.Read

-- | Prints `msg` and then waits for the user to enter a `String`.
promptLine :: String -> IO String
promptLine msg = do
    putStr msg
    hFlush stdout
    getLine

-- | Same as `promptLine` but splits the `String` into a list.
promptWords :: String -> IO [String]
promptWords msg = words <$> promptLine msg

-- | Prints `msg`, waits for the user to enter a `String`, and then attempts to parse it into `a` via `Text.Read.readMaybe`.
input :: Read a => String -> IO (Maybe a)
input msg = (Text.Read.readMaybe :: Read a => String -> Maybe a) <$> promptLine msg

-- | Prompts the user to enter a value of `a` and checks it against a validator `a -> Bool` until parsing and validation succeeds.
prompt :: Read a => String -> (a -> Bool) -> IO a
prompt msg validator = do
    res <- input msg
    case res of
        Just x -> 
            if validator x 
                then return x 
                else repeat
        Nothing -> repeat
    where 
        repeat = do
            putStrLn "Invalid input."
            prompt msg validator

-- | Convenience version for Int.
promptInt :: String -> (Int -> Bool) -> IO Int
promptInt msg validator = (prompt :: String -> (Int -> Bool) -> IO Int) msg validator

-- | Convenience version for Double.
promptDouble :: String -> (Double -> Bool) -> IO Double
promptDouble msg validator = (prompt :: String -> (Double -> Bool) -> IO Double) msg validator

-- | Prints `msg` and then asks the user to enter `y | yes | n | no`. If the user gives anything else, this function repeats.
yesOrNo :: String -> IO Bool
yesOrNo msg = do
    str <- promptLine $ msg ++ " (y / n): "
    case map toLower str of
        ('y' : _) -> return True  -- catches "y" and "yes"
        ('n' : _) -> return False -- catches "n" and "no"
        _   -> do
            putStrLn "Invalid input."
            yesOrNo msg