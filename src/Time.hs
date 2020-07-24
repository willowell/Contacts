module Time where

import Data.Default
import Data.Time
import Data.Tuple.HT

data GregorianDate = GregorianDate {
        year :: Integer
    ,   month :: Int
    ,   day :: Int
} deriving (Eq, Read, Show)

instance Default GregorianDate where
    def = GregorianDate 0 0 0

{-
stringifyDate :: GregorianDate -> String
stringifyDate date = formatTime defaultTimeLocale "%A %d %B, %Y" $ uncurry3 fromGregorian $ date
-}

stringifyDay :: UTCTime -> String
stringifyDay day = formatTime defaultTimeLocale "%A %d %B, %Y" $ day

now :: IO String
now = stringifyDay <$> getCurrentTime

whatTimeIsIt :: IO ()
whatTimeIsIt = do
    now <- getCurrentTime
    putStrLn $ ("It is currently: " ++) $ show $ formatTime defaultTimeLocale "%A %d %B, %Y" now