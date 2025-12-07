{-# LANGUAGE InstanceSigs #-}

module Day03 (Day03) where

import Base (Day (..))
import Data.List (tails, uncons)
import Data.Maybe (fromJust, fromMaybe, isNothing, listToMaybe)
import Data.Ord (comparing)

data Day03

instance Day Day03 where
  type ParsedData Day03 = Data

  dayNumber :: Int
  dayNumber = 3
  parseInput :: String -> Data
  parseInput = Data . lines
  part1 :: Data -> Integer
  part1 = doPart1
  part2 :: Data -> Integer
  part2 = doPart2

-- ### Parsing ###

newtype Data = Data {getData :: [[Char]]}

-- ### Part 1 ###

doPart1 :: Data -> Integer
doPart1 = sum . map (read . findMax 2) . getData

findMax :: Int -> [Char] -> [Char]
findMax n ds
  | n == 0 = []
  | len <= n = ds
  | isNothing bestTail = ds
  | otherwise = d : findMax (n - 1) ds'
  where
    len = length ds
    bestTail = uncons . maximumByLeft (comparing listToMaybe) . take (len - n + 1) . tails $ ds
    (d, ds') = fromJust bestTail

-- Like Data.List maximumBy, but favours leftwards for ties
maximumByLeft :: (Foldable t) => (a -> a -> Ordering) -> t a -> a
maximumByLeft cmp =
  fromMaybe (errorWithoutStackTrace "maximumBy: empty structure")
    . foldl' max' Nothing
  where
    max' mx y =
      Just $! case mx of
        Nothing -> y
        Just x -> case cmp x y of
          LT -> y
          _ -> x

-- ### Part 2 ###

doPart2 :: Data -> Integer
doPart2 = sum . map (read . findMax 12) . getData
