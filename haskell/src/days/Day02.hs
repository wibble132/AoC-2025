{-# LANGUAGE InstanceSigs #-}

module Day02 (Day02) where

import Base (Day (..), fromRight')
import Data.List (nub)
import Text.Parsec (Parsec, char, digit, many, parse, sepBy)

data Day02

instance Day Day02 where
  type ParsedData Day02 = Data

  dayNumber :: Int
  dayNumber = 2
  parseInput :: String -> Data
  parseInput = fromRight' . parse doParse ""
  part1 :: Data -> Integer
  part1 = doPart1
  part2 :: Data -> Integer
  part2 = doPart2

-- ### Parsing ###

newtype Data = Data [(Integer, Integer)]

doParse :: Parsec String () Data
doParse = Data <$> parsePair `sepBy` char ','

parsePair :: Parsec String () (Integer, Integer)
parsePair = do
  a <- many digit
  _ <- char '-'
  b <- many digit
  pure (read a, read b)

-- ### Part 1 ###

doPart1 :: Data -> Integer
doPart1 (Data xs) = sum . map (uncurry countValids) $ xs

countValids :: Integer -> Integer -> Integer
countValids a b = sum . filter (isRepeated 2) $ [a .. b]

isRepeated :: Int -> Integer -> Bool
isRepeated n x
  | l `mod` n /= 0 = False
  | otherwise = length distincts == 1
  where
    s = show x
    l = length s
    chunkSize = l `div` n
    cks = chunks chunkSize s
    distincts = nub cks

chunks :: Int -> [a] -> [[a]]
chunks x ls = take x ls : rest'
  where
    rest = drop x ls
    rest'
      | null rest = []
      | otherwise = chunks x rest

-- ### Part 2 ###

doPart2 :: Data -> Integer
doPart2 (Data xs) = sum . map (uncurry countValids2) $ xs

countValids2 :: Integer -> Integer -> Integer
countValids2 a b = sum . filter isAnyRepeated $ [a .. b]

isAnyRepeated :: Integer -> Bool
isAnyRepeated x = any (`isRepeated` x) [2 .. 20]
