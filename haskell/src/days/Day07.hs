{-# LANGUAGE InstanceSigs #-}

module Day07 (Day07) where

import Data.List (elemIndex, sort, sortOn, nub)
import Data.Maybe (fromJust)

import Base (Day (..))

data Day07

instance Day Day07 where
  type ParsedData Day07 = Data

  dayNumber :: Int
  dayNumber = 7
  parseInput :: String -> Data
  parseInput = parse

  part1 :: Data -> Integer
  part1 = toInteger . doPart1
  part2 :: Data -> Integer
  part2 = toInteger . doPart2

-- ### Parsing ###

data Data = Data [[Tile]]

data Tile = Start | Empty | Splitter deriving Eq

parse :: String -> Data
parse = Data . map (map charToTile) . lines

charToTile :: Char -> Tile
charToTile '.' = Empty
charToTile '^' = Splitter
charToTile 'S' = Start
charToTile c = error $ "Unknown " ++ [c]


-- ### Part 1 ###

doPart1 :: Data -> Int
doPart1 (Data tiles) = fst $ foldl' doStep (0, [startIndex]) tiles
  where
    startIndex :: Int
    startIndex = fromJust . elemIndex Start $ head tiles

    doStep :: (Int, [Int]) -> [Tile] -> (Int, [Int])
    doStep (x, currentBeams) row = (x + length splitBeams, nextBeams)
        where
            splitBeams :: [Int]
            splitBeams = filter ((== Splitter) . (row !!)) currentBeams
            passThruBeams :: [Int]
            passThruBeams = filter ((/= Splitter) . (row !!)) currentBeams
            splitBeamResults :: [Int]
            splitBeamResults = concatMap (\x -> [x-1, x+1]) splitBeams

            nextBeams :: [Int]
            nextBeams = nub . sort $ (splitBeamResults ++ passThruBeams)


-- ### Part 2 ###

doPart2 :: Data -> Int
doPart2 (Data tiles) = sum . map fst $ foldl' doStep ([(1, startIndex)]) tiles
  where
    startIndex :: Int
    startIndex = fromJust . elemIndex Start $ head tiles

    doStep :: ([(Int, Int)]) -> [Tile] -> ([(Int, Int)])
    doStep currentBeams row = nextBeams
        where
            splitBeams :: [(Int, Int)]
            splitBeams = filter ((== Splitter) . (row !!) . snd) currentBeams
            passThruBeams :: [(Int, Int)]
            passThruBeams = filter ((/= Splitter) . (row !!) . snd) currentBeams
            splitBeamResults :: [(Int, Int)]
            splitBeamResults = concatMap (\(n, x) -> [(n, x-1), (n, x+1)]) splitBeams

            nextBeams :: [(Int, Int)]
            nextBeams = combineBeams . sortOn snd $ (splitBeamResults ++ passThruBeams)

combineBeams :: [(Int, Int)] -> [(Int, Int)]
combineBeams [] = []
combineBeams (b : []) = b : []
combineBeams (b1 : b2 : bs)
  | snd b1 == snd b2 = combineBeams $ (fst b1 + fst b2, snd b1) : bs
  | otherwise = b1 : combineBeams (b2 : bs)
