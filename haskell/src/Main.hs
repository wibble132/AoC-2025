{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Base (Day (..), readInputFile)
import Data.Maybe (listToMaybe)
import Day01 (Day01)
import Day02 (Day02)
import Day03 (Day03)
import Day05 (Day05)
import Day06 (Day06)
import Language.Haskell.TH (Exp, Quote)
import System.Environment (getArgs)
import Text.Read (readMaybe)

main :: IO ()
main = do
  args <- getArgs
  let selectedDay :: Maybe Int = readMaybe =<< listToMaybe args

  maybe allDays runDayN selectedDay

allDays :: IO ()
allDays = mapM_ runDayN ([1 .. 3] ++ [5])

runDayN :: Int -> IO ()
runDayN 1 = runDay @Day01
runDayN 2 = runDay @Day02
runDayN 3 = runDay @Day03
runDayN 5 = runDay @Day05
runDayN 6 = runDay @Day06
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

oneC :: (Quote m) => m Exp
oneC = [|1|]

twoC :: (Quote m) => m Exp
twoC = [|2|]

plusC :: (Quote m) => m Exp
plusC = [|$oneC + $twoC|]
