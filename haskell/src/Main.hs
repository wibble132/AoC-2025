{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import System.Environment (getArgs)
import Data.Maybe (listToMaybe)
import Text.Read (readMaybe)
import Control.Monad (void)

import Day01 (part1, part2, parseInput)
import Day02 (part1, part2, parseInput)

main :: IO ()
main = do
    args <- getArgs
    let selectedDay :: Maybe Int = readMaybe =<< listToMaybe args

    maybe (allDays) (runDay) selectedDay

allDays :: IO ()
allDays = void $ mapM runDay [1..12]

runDay :: Int -> IO ()
runDay n = do
    inputText <- readInputFile n

    putStrLn $ "Day " ++ show n
    putStrLn ""
    
    let (p1, p2) = runDayParts n inputText
    putStrLn $ "Part 1: " ++ show p1
    putStrLn $ "Part 2: " ++ show p2
    putStrLn ""

readInputFile :: Int -> IO String
readInputFile n = readFile fileName
    where
        nStr = show n

        pad :: String -> String
        pad [c] = ['0', c]
        pad cs = cs

        fileName :: FilePath
        fileName = "../inputs/day" ++ pad nStr ++ ".txt"

runDayParts :: Int -> String -> (Integer, Integer)
runDayParts 1 s = (toInteger $ Day01.part1 d, toInteger $ Day01.part2 d) where d = Day01.parseInput s
runDayParts 2 s = (Day02.part1 d, Day02.part2 d) where d = Day02.parseInput s
runDayParts n _ = error $ "Unknown day: " ++ show n
