module Day11 (Day11) where

import Base (Day (..), fromRight')
import Data.Bifunctor (second)
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Parsec (Parsec, char, letter, many, newline, optional, parse, sepBy)

data Day11

instance Day Day11 where
  type ParsedData Day11 = Data

  dayNumber :: Int
  dayNumber = 11
  parseInput :: String -> Data
  parseInput = fromRight' . parse doParse ""

  part1 :: Data -> Integer
  part1 = toInteger . doPart1
  part2 :: Data -> Integer
  part2 = toInteger . doPart2

-- ### Parsing ###

newtype Data = Data (Map Device [Device])

type Device = String

doParse :: Parsec String () Data
doParse = Data . Map.fromList <$> many (parseLine <* optional newline)

parseLine :: Parsec String () (Device, [Device])
parseLine = do
  d <- parseDevice
  _ <- char ':'
  _ <- char ' '
  ds <- parseDevice `sepBy` char ' '
  pure (d, ds)

parseDevice :: Parsec String () Device
parseDevice = many letter

-- ### Part 1 ###

doPart1 :: Data -> Int
doPart1 (Data d) = length . pathsDfs "you" (== "out") $ \k -> Map.findWithDefault [] k d

pathsDfs :: (Show a) => a -> (a -> Bool) -> (a -> [a]) -> [[a]]
pathsDfs start isEnd nextSteps
  | isEnd start = [[start]]
  | otherwise =
      [ next : end
      | next <- nextSteps start,
        end <- pathsDfs next isEnd nextSteps
      ]

-- ### Part 2 ###

doPart2 :: Data -> Int
doPart2 (Data d)
  | fftToDac /= 0 = paths2
  | dacToFft /= 0 = paths1
  | otherwise = 0
  where
    pathsFromTo :: Device -> Device -> Int
    pathsFromTo from to = pathsDfsCount from (== to) $ \k -> Map.findWithDefault [] k d

    -- Possible paths are svr -> dac -> fft -> out or svr -> fft -> dac -> out
    -- But note: one of `dac -> fft` or `fft -> dac` MUST be zero, since we the graph doesn't have loops (else result is infinite)
    -- So if we calculate those first, we save the rest
    dacToFft = pathsFromTo "dac" "fft"
    fftToDac = pathsFromTo "fft" "dac"
    paths1 = pathsFromTo "svr" "dac" * dacToFft * pathsFromTo "fft" "out"
    paths2 = pathsFromTo "svr" "fft" * fftToDac * pathsFromTo "dac" "out"

pathsDfsCount :: forall a. (Ord a) => a -> (a -> Bool) -> (a -> [a]) -> Int
pathsDfsCount start isEnd nextSteps = snd $ doSearch Map.empty start
  where
    doSearch :: Map a Int -> a -> (Map a Int, Int)
    doSearch cache from
      | Just x <- Map.lookup from cache = (cache, x)
      | isEnd from = (Map.singleton from 1, 1)
      | otherwise = updateCache $ foldl' foldStep (cache, 0) $ nextSteps from
      where
        foldStep :: (Map a Int, Int) -> a -> (Map a Int, Int)
        foldStep (c, t) f = second (+ t) $ doSearch c f

        updateCache :: (Map a Int, Int) -> (Map a Int, Int)
        updateCache (c, t) = (Map.insert from t c, t)
