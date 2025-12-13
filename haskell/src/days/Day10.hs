{-# OPTIONS_GHC -Wno-x-partial -Wno-unrecognised-warning-flags #-}

module Day10 (Day10) where

import Base (Day (..), fromRight', number)
import Data.List (find, findIndex, sortOn, transpose)
import Data.Maybe (catMaybes, fromJust, listToMaybe)
import Data.Ratio (denominator, numerator)
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Parsec (Parsec, char, many, newline, parse, sepBy, space, (<|>))

data Day10

instance Day Day10 where
  type ParsedData Day10 = Data

  dayNumber :: Int
  dayNumber = 10
  parseInput :: String -> Data
  parseInput = fromRight' . parse doParse ""

  part1 :: Data -> Integer
  part1 = toInteger . doPart1
  part2 :: Data -> Integer
  part2 = toInteger . doPart2

-- ### Parsing ###

newtype Data = Data [MachineDescription]

data MachineDescription = MD
  { targetLights :: [LightState],
    buttonWiringSchematics :: [ButtonWiringSchematic],
    joltageRequirements :: [Int]
  }
  deriving (Show)

data LightState = On | Off deriving (Show, Eq, Ord)

type ButtonWiringSchematic = [Int]

doParse :: Parsec String () Data
doParse = Data <$> many (description <* newline)

description :: Parsec String () MachineDescription
description = do
  _ <- char '['
  lights <- many ((char '.' >> pure Off) <|> (char '#' >> pure On))
  _ <- char ']'
  _ <- space

  buttons <- many (char '(' >> (number `sepBy` char ',') <* char ')' <* char ' ')

  _ <- char '{'
  joltages <- number `sepBy` char ','
  _ <- char '}'

  pure $ MD lights buttons joltages

-- ### Part 1 ###

doPart1 :: Data -> Int
doPart1 (Data d) = sum . map turnOnMachine $ d

flipLight :: LightState -> LightState
flipLight On = Off
flipLight Off = On

pressButton :: [LightState] -> [Int] -> [LightState]
pressButton ls' ns' = doPress ls' ns' 0
  where
    doPress :: [LightState] -> [Int] -> Int -> [LightState]
    doPress [] _ _ = []
    doPress ls [] _ = ls
    doPress (l : ls) (n : ns) m
      | m == n = flipLight l : doPress ls ns (m + 1)
      | otherwise = l : doPress ls (n : ns) (m + 1)

turnOnMachine :: MachineDescription -> Int
turnOnMachine (MD target buttons _) = searchStep (Set.singleton startLights) [startLights] 0
  where
    lightCount = length target
    startLights = replicate lightCount Off

    searchStep :: Set [LightState] -> [[LightState]] -> Int -> Int
    searchStep visited searchStarts n
      | target `Set.member` visited = n
      | otherwise = searchStep nextVisited nextSearchStarts (n + 1)
      where
        nextSearchStarts =
          concatMap (filter (`Set.notMember` visited) . (\s -> map (pressButton s) buttons)) searchStarts

        nextVisited = Set.union visited (Set.fromList nextSearchStarts)

-- ### Part 2 ###

doPart2 :: Data -> Int
doPart2 (Data d) = sum . map solve2 $ d

solve2 :: MachineDescription -> Int
solve2 (MD _ buttons target) = sum $ solveFromEliminated dim eliminated
  where
    dim = length target
    buttonsMatrix = buttonsToMatrix dim (sortOn length buttons)
    eliminated :: [(Int, [Rational], Rational)]
    eliminated = gaussianElimination dim buttonsMatrix (map fromIntegral target)

-- So, this can be represented as a Matrix equation
-- where each button represents a column of a matrix M - e.g. (1,2,5) represents (0,1,1,0,0,1,...)
-- And the result we want is an `x` satisfying `Mx = joltageTarget`, with minimised sum of components of `x`
--
-- e.g. [.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
-- matrix M is:
-- [ 0 0 0 0 1 1 ]    [3]
-- [ 0 1 0 0 0 1 ]x = [5]
-- [ 0 0 1 1 1 0 ]    [4]
-- [ 1 1 0 1 0 0 ]    [7]

-- This is rearranged in rows to get (can also add/sub rows if needed)
-- [ 1 1 0 1 0 0 ]    [7]
-- [ 0 1 0 0 0 1 ]x = [5]
-- [ 0 0 1 1 1 0 ]    [4]
-- [ 0 0 0 0 1 1 ]    [3]

-- The first 1 in each row constrains a component of x (here, x0, x1, x2, x4) and the others are left up to a range (0.. [max allowed by equation])

----- Gaussian Elimination -----

buttonsToMatrix :: Int -> [ButtonWiringSchematic] -> [[Rational]]
buttonsToMatrix buttonCount = transpose . map (take buttonCount . map fromIntegral . buttonToRow 0)

buttonToRow :: Int -> [Int] -> [Int]
buttonToRow _ [] = repeat 0
buttonToRow n (x : xs)
  | n == x = 1 : buttonToRow (n + 1) xs
  | otherwise = 0 : buttonToRow (n + 1) (x : xs)

gaussianElimination :: Int -> [[Rational]] -> [Rational] -> [(Int, [Rational], Rational)]
gaussianElimination dim m' r' = valsFromSteps
  where
    valsFromSteps :: [(Int, [Rational], Rational)]
    valsFromSteps = map f [0 .. (dim - 1)]
      where
        f :: Int -> (Int, [Rational], Rational)
        f n = (\(a, b, m, r) -> (b, m !! (n - a), r !! (n - a))) $ last (filter (\(a, _, _, _) -> a <= n) steps)

    steps :: [(Int, Int, [[Rational]], [Rational])]
    steps = takeWhile (\(_, _, m, _) -> not (any null m) && not (null m)) $ iterate doStep (0, 0, m', r')

    doStep :: (Int, Int, [[Rational]], [Rational]) -> (Int, Int, [[Rational]], [Rational])
    doStep (a, b, m, r) = case actionToTake m of
      Unconstrained -> (a, b + 1, map tail m, r)
      Continue -> (a + 1, b + 1, tail $ map tail m, tail r)
      Swap swapIndex -> (a, b, doSwap swapIndex m, doSwap swapIndex r)
      Add mult n1 n2 -> (a, b, doAddMat mult n1 n2 m, doAddRes mult n1 n2 r)

doSwap :: Int -> [a] -> [a]
doSwap _ [] = error "Failed to swap"
doSwap n (x : xs) = (xs !! (n - 1)) : (take (n - 1) xs ++ (x : drop n xs))

doAddMat :: Rational -> Int -> Int -> [[Rational]] -> [[Rational]]
doAddMat mult from to mat = take to mat ++ (addResult : drop (to + 1) mat)
  where
    addAmount = map (* mult) (mat !! from)
    addResult = zipWith (+) addAmount (mat !! to)

doAddRes :: Rational -> Int -> Int -> [Rational] -> [Rational]
doAddRes mult from to res = take (to - 1) res ++ (addResult : drop to res)
  where
    addAmount = mult * (res !! from)
    addResult = addAmount + (res !! to)

actionToTake :: [[Rational]] -> EliminationAction
actionToTake m
  -- The first column is all zero
  | all (== 0) column = Unconstrained
  -- Top left nonzero, rest of column is zero
  | topLeft /= 0 && all (== 0) (tail column) = Continue
  -- Top left is 0, swap with first non-zero
  | topLeft == 0 = Swap firstNonZeroIndex
  -- Top left nonzero, another nonzero
  | otherwise = Add (-(secondNonZero / firstNonZero)) firstNonZeroIndex secondNonZeroIndex
  where
    column = map head m
    topLeft = head column
    firstNonZeroIndex = fromJust $ findIndex (/= 0) column
    secondNonZeroIndex = firstNonZeroIndex + 1 + fromJust (findIndex (/= 0) (drop (firstNonZeroIndex + 1) column))
    firstNonZero = column !! firstNonZeroIndex
    secondNonZero = column !! secondNonZeroIndex

data EliminationAction
  = Unconstrained -- This column is all zero, nothing to be done
  | Swap Int -- Swap row 0 with {0}
  | Add Rational Int Int -- Add {0} times {1} to {2}
  | Continue -- This column starts with 1 and then is all zero
  deriving (Show)

----- Solving from Eliminated Form -----

data SolveStep
  = Unbounded
  | Fixed ([Int] -> Maybe Int)

isUnbounded :: SolveStep -> Bool
isUnbounded Unbounded = True
isUnbounded _ = False

solveFromEliminated :: Int -> [(Int, [Rational], Rational)] -> [Int]
solveFromEliminated dim ms =
  let solvers = map (\(a, m, r) -> (a, getSolver m r)) ms
      solveSteps = map (\i -> maybe Unbounded (Fixed . snd) . find ((== i) . fst) $ solvers) [0 .. dim - 1]
      unboundedCount = length . filter isUnbounded $ solveSteps

      -- Try the smallest numbers first!
      unboundedAttempts = concatMap (itemsSummingTo unboundedCount) [0 ..]
      solveAttempts = map (attemptSolve solveSteps) unboundedAttempts
      firstSolve = catMaybes solveAttempts
   in head firstSolve
  where
    getSolver :: [Rational] -> Rational -> ([Int] -> Maybe Int)
    getSolver matRow result
      | listToMaybe matRow /= Just 1 = error "eliminated matrix sub row should start with 1"
      | otherwise = f
      where
        getTargetValue :: [Int] -> Rational
        getTargetValue = (result -) . sum . zipWith (*) matRow . map fromIntegral

        f :: [Int] -> Maybe Int
        f xs = case getTargetValue xs of
          x | denominator x == 1 && x > 0 -> Just . fromInteger . numerator $ x
          _ -> Nothing

itemsSummingTo :: Int -> Int -> [[Int]]
itemsSummingTo 1 tot = [[tot]]
itemsSummingTo itemCount 0 = [replicate itemCount 0]
itemsSummingTo itemCount tot = concatMap (\i -> map (i :) $ itemsSummingTo (itemCount - 1) (tot - i)) [0 .. tot]

attemptSolve :: [SolveStep] -> [Int] -> Maybe [Int]
attemptSolve [] [] = Just []
attemptSolve [] _ = error "Extra unbounded values!"
attemptSolve (Unbounded : _) [] = error "Missing unbounded steps!"
attemptSolve (Unbounded : steps) (u : us) = case attemptSolve steps us of
  Nothing -> Nothing
  Just xs -> Just (u : xs)
attemptSolve (Fixed f : steps) us = case attemptSolve steps us of
  Nothing -> Nothing
  Just xs -> (: xs) <$> f xs