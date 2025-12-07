{-# LANGUAGE InstanceSigs #-}

module Day06 (Day06) where

import Base (Day (..))
import Data.Bifunctor (bimap, first)
import Data.List (transpose, unsnoc)
import Data.List.Split (splitWhen)
import Data.Maybe (catMaybes, fromJust)

data Day06

instance Day Day06 where
  type ParsedData Day06 = Data

  dayNumber :: Int
  dayNumber = 6
  parseInput :: String -> Data
  parseInput = parse

  part1 :: Data -> Integer
  part1 = toInteger . doPart1 . getData
  part2 :: Data -> Integer
  part2 = toInteger . doPart2 . getData

-- ### Parsing ###

newtype Data = Data {getData :: [([[Maybe Digit]], Operator)]}

data Problem = Problem [Int] Operator

data Operator = Plus | Times

data Digit = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9

parse :: String -> Data
parse = Data . map (bimap (map $ map charToDigit) (parseOperator . filter (/= ' ')) . fromJust . unsnoc . transpose) . splitWhen (all (== ' ')) . transpose . lines

charToDigit :: Char -> Maybe Digit
charToDigit ' ' = Nothing
charToDigit c = Just $ case c of
  '0' -> D0
  '1' -> D1
  '2' -> D2
  '3' -> D3
  '4' -> D4
  '5' -> D5
  '6' -> D6
  '7' -> D7
  '8' -> D8
  '9' -> D9
  _ -> undefined

parseOperator :: String -> Operator
parseOperator "+" = Plus
parseOperator "*" = Times
parseOperator c = error $ "Unknown operator " ++ c

-- ### Part 1 ###

evaluate :: Problem -> Int
evaluate (Problem nums Plus) = sum nums
evaluate (Problem nums Times) = product nums

digitToInt :: Digit -> Int
digitToInt D0 = 0
digitToInt D1 = 1
digitToInt D2 = 2
digitToInt D3 = 3
digitToInt D4 = 4
digitToInt D5 = 5
digitToInt D6 = 6
digitToInt D7 = 7
digitToInt D8 = 8
digitToInt D9 = 9

readDigits :: [Digit] -> Int
readDigits = foldl' (\acc d -> acc * 10 + digitToInt d) 0

doPart1 :: [([[Maybe Digit]], Operator)] -> Int
doPart1 = sum . map (evaluate . uncurry Problem . first (map (readDigits . catMaybes)))

-- ### Part 2 ###

doPart2 :: [([[Maybe Digit]], Operator)] -> Int
doPart2 = doPart1 . map (first transpose)
