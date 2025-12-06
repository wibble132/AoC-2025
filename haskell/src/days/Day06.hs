module Day06 (Day06) where

import Base (Day (..))
import Data.List (transpose, unsnoc)
import Data.List.Split (splitWhen)
import Data.Maybe (fromJust)
import Data.Bifunctor (first, second)

data Day06

instance Day Day06 where
  type ParsedData Day06 = Data

  dayNumber = 6
  parseInput = parse

  part1 = toInteger . doPart1 . getData
  part2 = toInteger . doPart2 . getData

-- ### Parsing ###

newtype Data = Data {getData:: [([String], Operator)]}

data Problem = Problem [Int] Operator 

data Operator = Plus | Times

parse :: String -> Data
parse = Data . map (second (parseOperator . filter (/= ' ')) . fromJust . unsnoc . transpose) . splitWhen (all (== ' ')) . transpose . lines

parseOperator :: String -> Operator
parseOperator "+" = Plus
parseOperator "*" = Times
parseOperator c = error $ "Unknown operator " ++ c

-- ### Part 1 ###

evaluate :: Problem -> Int
evaluate (Problem nums Plus) = sum nums
evaluate (Problem nums Times) = product nums

doPart1 :: [([String], Operator)] -> Int
doPart1 = sum . map (evaluate . uncurry Problem . first (map (read :: String -> Int)))

-- ### Part 2 ###

doPart2 :: [([String], Operator)] -> Int
doPart2 = doPart1 . map (first transpose)