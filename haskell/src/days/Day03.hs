module Day03 (part1, part2, parseInput) where

import Data.List (tails, uncons)
import Data.Maybe (listToMaybe, isNothing, fromJust, fromMaybe)
import Data.Ord (comparing)

parseInput :: String -> Data
parseInput = lines

part1 :: Data -> Integer
part1 = doPart1

part2 :: Data -> Integer
part2 = doPart2

-- ### Parsing ###

type Data = [[Char]]

-- ### Part 1 ###

doPart1 :: Data -> Integer
doPart1 = sum . map (read . findMax 2)

findMax :: Int -> [Char] -> [Char]
findMax n ds
 | n == 0 = []
 | len <= n = ds
 | isNothing bestTail = ds
 | otherwise = d : (findMax (n - 1) ds')
 where
    len = length ds
    bestTail = uncons . maximumByLeft (comparing (listToMaybe)) . take (len - n + 1) . tails $ ds
    (d, ds') = fromJust bestTail

-- Like Data.List maximumBy, but favours leftwards for ties
maximumByLeft :: Foldable t => (a -> a -> Ordering) -> t a -> a
maximumByLeft cmp = fromMaybe (errorWithoutStackTrace "maximumBy: empty structure")
  . foldl' max' Nothing
  where
    max' mx y = Just $! case mx of
      Nothing -> y
      Just x -> case cmp x y of
        LT -> y
        _ -> x

-- ### Part 2 ###

doPart2 :: Data -> Integer
doPart2 = sum . map (read . findMax 12)