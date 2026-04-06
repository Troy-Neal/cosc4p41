module COSC4P41(Long,nanosSinceEpoch,split,trim,subSet) where

import Data.Char
import Data.Int
import Data.List
import Data.Time.Clock
import Data.Time.Clock.POSIX

type Long = Int64

nanosSinceEpoch :: IO Long
nanosSinceEpoch = do time <- getCurrentTime
                     return $ (floor . (1e9 *) . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds) time
                     
split :: Eq a => a -> [a] -> [[a]]
split x xs = if null rest then [first] else first:split x (tail rest)
  where (first,rest) = break (==x) xs
  
trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

subSet :: Eq a => [a] -> [a] -> Bool
subSet l1 l2 = all (`elem` l2) l1
