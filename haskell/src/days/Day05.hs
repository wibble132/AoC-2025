{-# LANGUAGE InstanceSigs #-}

module Day05 (Day05) where

import Base (Day (..), fromRight')
import Data.Int (Int64)
import Data.List (sort)
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Parsec (Parsec, char, digit, many, many1, newline, parse)

data Day05

instance Day Day05 where
  type ParsedData Day05 = Data

  dayNumber :: Int
  dayNumber = 5
  parseInput :: String -> Data
  parseInput = fromRight' . parse doParse ""
  part1 :: Data -> Integer
  part1 = toInteger . doPart1
  part2 :: Data -> Integer
  part2 = toInteger . doPart2

-- ### Parsing ###

data Data = Data [Range] [Int64]

data Range = Range Int64 Int64
  deriving (Eq, Ord, Show)

doParse :: Parsec String () Data
doParse = do
  ranges <- many (parseRange <* newline)
  _ <- newline
  ids <- many (number <* newline)

  pure $ Data ranges ids

parseRange :: Parsec String () Range
parseRange = (char ('-') >>) . (<$> number) . Range =<< number

number :: Parsec String () Int64
number = do
  digits <- many1 digit
  pure $ read digits

-- ### Part 1 ###

contractRanges :: [Range] -> [Range]
contractRanges = doContract . sort
  where
    doContract [] = []
    doContract [r] = [r]
    doContract (r1@(Range a1 b1) : r2@(Range a2 b2) : rs)
      | b2 < b1 = doContract $ r1 : rs
      | b1 < a2 = r1 : doContract (r2 : rs)
      | otherwise = doContract $ Range a1 b2 : rs

doPart1 :: Data -> Int
doPart1 (Data ranges ids) = length . filter isValid $ ids
  where
    -- The precondition for the list being strictly increasing comes from contractRanges
    rangeSet :: Set Range
    rangeSet = Set.fromDistinctAscList $ contractRanges ranges

    isValid :: Int64 -> Bool
    isValid i = case Set.lookupLE (Range i i) rangeSet of
      Nothing -> False
      Just (Range _ b) -> b >= i

-- ### Part 2 ###

size :: Range -> Int64
size (Range a b) = (b - a + 1)

doPart2 :: Data -> Int64
doPart2 (Data ranges _) = sum $ map size $ contractRanges ranges
