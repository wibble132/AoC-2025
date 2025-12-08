{-# LANGUAGE InstanceSigs #-}

module Day07 (Day07) where

import Data.List (elemIndex, elemIndices, sort, sortOn, nub, uncons)
import Data.Maybe (fromJust)
import Data.Bifunctor (bimap)

import Data.Vector (Vector)
import qualified Data.Vector as Vector

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

data Data = Data { startRow :: Vector Bool, splitters :: [Vector Bool]} deriving Show

data Tile = Start | Empty | Splitter deriving Eq

parse :: String -> Data
parse = uncurry Data . bimap (Vector.fromList . map (== Start)) (map Vector.fromList . filter (not . all id) . map (map (== Splitter))) . fromJust . uncons . map (map charToTile) . lines

charToTile :: Char -> Tile
charToTile '.' = Empty
charToTile '^' = Splitter
charToTile 'S' = Start
charToTile c = error $ "Unknown " ++ [c]

-- ### Part 1 ###

doPart1 :: Data -> Int
doPart1 d = fst . foldl' (uncurry doStep) (0, startRow d) . splitters $ d
  where
    rowLength :: Int
    rowLength = Vector.length . startRow $ d

    doStep :: Int -> Vector Bool -> Vector Bool -> (Int, Vector Bool)
    doStep splits currentBeams row = (splits + newSplits, Vector.generate rowLength isInNextBeam)
      where
        isInNextBeam :: Int -> Bool
        isInNextBeam x
          | (currentBeams Vector.! x) && not (row Vector.! x) = True
          | x > 0 && (currentBeams Vector.! (x - 1)) && row Vector.! (x - 1) = True
          | x + 1 < rowLength && (currentBeams Vector.! (x + 1)) && row Vector.! (x + 1) = True
          | otherwise = False

        newSplits :: Int
        newSplits = length . filter (uncurry (&&)) $ (Vector.toList currentBeams) `zip` (Vector.toList row)

-- ### Part 2 ###

doPart2 = const (0::Int)

-- doPart2 :: Data -> Int
-- doPart2  d = sum . map fst . foldl doStep [(1, startIndex)] . splitters $ d
--   where
--     startIndex :: Int
--     startIndex = startPos d

--     doStep :: [(Int, Int)] -> [Int] -> [(Int, Int)]
--     doStep currentBeams row = nextBeams
--       where
--         splitBeams :: [(Int, Int)]
--         splitBeams = filter ((`elem` row) . snd) currentBeams
--         passThruBeams :: [(Int, Int)]
--         passThruBeams = filter (not . (`elem` row) . snd) currentBeams
--         splitBeamResults :: [(Int, Int)]
--         splitBeamResults = concatMap (\(n, x) -> [(n, x-1), (n, x+1)]) splitBeams

--         nextBeams :: [(Int, Int)]
--         nextBeams = combineBeams . sortOn snd $ (splitBeamResults ++ passThruBeams)

-- combineBeams :: [(Int, Int)] -> [(Int, Int)]
-- combineBeams [] = []
-- combineBeams [b] = [b]
-- combineBeams (b1 : b2 : bs)
--   | snd b1 == snd b2 = combineBeams $ (fst b1 + fst b2, snd b1) : bs
--   | otherwise = b1 : combineBeams (b2 : bs)
