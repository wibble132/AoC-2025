module Main where

import Base (Day (..), readInputFile)
import Criterion.Main
import Day01 (Day01)
import Day02 (Day02)
import Day03 (Day03)
import Day05 (Day05)
import Day06 (Day06)

main :: IO ()
main = do
  days <-
    sequence
      [ benchDay @Day01,
        benchDay @Day02,
        benchDay @Day03,
        benchDay @Day05,
        benchDay @Day06
      ]

  defaultMain days

benchDay :: forall a. (Day a) => IO Benchmark
benchDay = do
  input <- readInputFile @a
  let parsed = (parseInput @a) input

  let groupName = "day" ++ show (dayNumber @a)

  pure $
    bgroup
      groupName
      [ bench "parse" $ whnf (parseInput @a) input,
        bench "part1" $ nf (part1 @a) parsed,
        bench "part2" $ nf (part2 @a) parsed
      ]
