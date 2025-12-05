module Day05 (Day05) where


import Text.Parsec (Parsec, parse, digit, many, many1, newline, char)
import Data.List (sort)

import Base (Day(..), fromRight')

data Day05
instance Day Day05 where
    type ParsedData Day05 = Data

    dayNumber = 5
    parseInput = fromRight' . parse doParse ""
    part1 = toInteger . doPart1
    part2 = doPart2

-- ### Parsing ###

data Data = Data [Range] [Integer]
data Range = Range Integer Integer
  deriving (Eq, Ord, Show)

doParse :: Parsec String () Data
doParse = do
    ranges <- many (parseRange <* newline)
    _ <- newline
    ids <- many (number <* newline)

    pure $ Data ranges ids
    
parseRange :: Parsec String () Range
parseRange = (char ('-') >>) . (<$> number) . Range =<< number

number :: Parsec String () Integer
number = do
    digits <- many1 digit
    pure $ read digits

-- ### Part 1 ###

doPart1 :: Data -> Int
doPart1 (Data ranges ids) = length . filter isValid $ ids
  where
    isValid :: Integer -> Bool
    isValid i = any (\(Range a b) -> a <= i && i <= b) ranges

-- ### Part 2 ###

size :: Range -> Integer
size (Range a b) = toInteger (b - a + 1)

doPart2 :: Data -> Integer
doPart2 (Data ranges _) = doStep $ sort ranges
  where
    doStep :: [Range] -> Integer
    doStep [] = 0
    doStep [r] = size r
    doStep (r1@(Range a1 b1) : r2@(Range a2 b2) : rs)
      | b2 < b1 = doStep $ r1 : rs
      | b1 < a2 = size r1 + doStep (r2 : rs)
      | otherwise = doStep $ Range a1 b2 : rs
