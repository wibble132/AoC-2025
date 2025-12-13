{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Base (Day (..), readInputFile)
import Data.Maybe (listToMaybe)
import Day01 (Day01)
import Day02 (Day02)
import Day03 (Day03)
import Day05 (Day05)
import Day06 (Day06)
import Day07 (Day07)
import Day08 (Day08)
import Day09 (Day09)
import Day10 (Day10)
import Day11 (Day11)
import System.Environment (getArgs)
import Text.Read (readMaybe)

main :: IO ()
main = do
  args <- getArgs
  let selectedDay :: Maybe Int = readMaybe =<< listToMaybe args

  maybe allDays runDayN selectedDay

allDays :: IO ()
allDays = mapM_ runDayN ([1 .. 3] ++ [5 .. 11])

runDayN :: Int -> IO ()
runDayN 1 = runDay @Day01
runDayN 2 = runDay @Day02
runDayN 3 = runDay @Day03
runDayN 5 = runDay @Day05
runDayN 6 = runDay @Day06
runDayN 7 = runDay @Day07
runDayN 8 = runDay @Day08
runDayN 9 = runDay @Day09
runDayN 10 = runDay @Day10
runDayN 11 = runDay @Day11
runDayN n = error $ "Unknown day: " ++ show n

runDay :: forall a. (Day a) => IO ()
runDay = do
  let n = dayNumber @a
  inputText <- readInputFile @a

  putStrLn $ "Day " ++ show n
  putStrLn ""

  let (p1, p2) = (runDayParts @a) inputText
  putStrLn $ "Part 1: " ++ show p1
  putStrLn $ "Part 2: " ++ show p2
  putStrLn ""

runDayParts :: forall a. (Day a) => String -> (Integer, Integer)
runDayParts input = (part1 @a parsed, part2 @a parsed)
  where
    parsed :: ParsedData a = (parseInput @a) input