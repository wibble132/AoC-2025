module Day01 (part1, part2, parseInput) where

import Data.Maybe (fromJust)

parseInput :: String -> Data
parseInput =  Data . fromJust . mapM parseRotation . lines

part1 :: Data -> Int
part1 = doPart1

part2 :: Data -> Int
part2 = doPart2

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