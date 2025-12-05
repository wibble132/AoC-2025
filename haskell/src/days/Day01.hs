module Day01 (Day01) where

import Data.Maybe (fromJust)

import Base (Day(..))

data Day01
instance Day Day01 where
    type ParsedData Day01 = Data

    dayNumber = 1
    parseInput = Data . fromJust . mapM parseRotation . lines
    part1 = toInteger . doPart1
    part2 = toInteger . doPart2

-- ### Parsing ###

newtype Data = Data [Rotation]

data Rotation = Rotation Direction Int

data Direction = L | R

parseRotation :: String -> Maybe Rotation
parseRotation (c : cs) = do
    d <- parseDirection c
    Just $ Rotation d (read cs)
parseRotation _ = Nothing

parseDirection :: Char -> Maybe Direction
parseDirection 'L' = Just L
parseDirection 'R' = Just R
parseDirection _ = Nothing

-- ### Part 1 ###

doPart1 :: Data -> Int
doPart1 (Data rots) = fst $ foldl' doRun (0, 50) rots

doRun :: (Int, Int) -> Rotation -> (Int, Int)
doRun (zeros, pos) (Rotation dir dist) = (zeros + fromEnum isZero, newPos)
  where
    newPos = case dir of 
      L -> pos + dist
      R -> pos - dist
    isZero = (newPos `rem` 100) == 0

-- ### Part 2 ###

doPart2 :: Data -> Int
doPart2 (Data rotations) = fst $ foldl' doRun (0, 50) rots
  where
    -- Just replate every rotation with `n` rotations of length 1
    rots :: [Rotation]
    rots = map (`Rotation` 1) $ concatMap (\(Rotation dir n) -> replicate n dir) rotations