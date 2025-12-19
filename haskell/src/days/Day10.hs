{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-x-partial -Wno-unrecognised-warning-flags #-}

module Day10 (Day10) where

import Base (Day (..), fromRight', number)
import Control.Lens (element, (.~))
import Control.Monad.ST (ST)
import Data.Bifunctor (Bifunctor (first, second))
import Data.Bits (Bits (xor), shiftL)
import Data.List (find, findIndex, sortOn, transpose, uncons)
import Data.Maybe (catMaybes, fromJust, fromMaybe, isNothing, listToMaybe)
import Data.Tuple (swap)
import Data.Vector.Mutable (STVector)
import qualified Data.Vector.Mutable as MVector
import Data.Word (Word64)
import GHC.Data.Word64Map.Strict (Word64Map)
import qualified GHC.Data.Word64Map.Strict as Word64Map
import Text.Parsec (Parsec, char, many, newline, parse, sepBy, sepEndBy1, (<|>))

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
doParse = Data <$> description `sepEndBy1` newline

description :: Parsec String () MachineDescription
description = do
  _ <- char '['
  lights <- many ((char '.' >> pure Off) <|> (char '#' >> pure On))
  _ <- char ']'
  _ <- char ' '

  buttons <- many (char '(' >> (number `sepBy` char ',') <* char ')' <* char ' ')

  _ <- char '{'
  joltages <- number `sepBy` char ','
  _ <- char '}'

  pure $ MD lights buttons joltages

-- ### Part 1 ###

type Lights = Word64

type Button = Word64

doPart1 :: Data -> Int
doPart1 (Data d) = sum . map turnOnMachine $ d

makeLights :: [LightState] -> Lights
makeLights = foldl' (\x l -> x `shiftL` 1 + case l of On -> 1; Off -> 0) 0

makeButton :: ButtonWiringSchematic -> Button
makeButton = sum . map (1 `shiftL`)

pressButton :: Lights -> Button -> Lights
pressButton = xor

turnOnMachine :: MachineDescription -> Int
turnOnMachine (MD target buttons _) = minimum $ Word64Map.elems middle
  where
    !lightCount = length target
    !trgtLights = makeLights . reverse $ target

    buttonValues :: [Button]
    buttonValues = map makeButton buttons

    (!leftButtons, !rightButtons) = splitAt (lightCount `div` 2) buttonValues
    !leftValues = searchAll 0 leftButtons
    !rightValues = searchAll trgtLights rightButtons

    !middle = Word64Map.intersectionWith (+) leftValues rightValues

    searchAll :: Word64 -> [Button] -> Word64Map Int
    searchAll n = foldl' goFold (Word64Map.singleton n 0)
      where
        goFold :: Word64Map Int -> Button -> Word64Map Int
        goFold m b =
          Word64Map.unionWith min m
            . Word64Map.map (+ 1)
            . Word64Map.mapKeys (`pressButton` b)
            $ m

-- ### Part 2 ###

-- Initial profiling: 2.741s - let's improve that
-- Change from Rational to Int -> 546.5 ms

doPart2 :: Data -> Int
doPart2 (Data d) = sum . map solve2 $ d

solve2 :: MachineDescription -> Int
solve2 (MD _ buttons target) = {-# SCC "solve" #-} sum $ solveFromEliminated dim2 (maximum target) eliminated
  where
    dim = length target
    buttonsMatrix = buttonsToMatrix dim (sortOn length buttons)
    dim2 = length . head $ buttonsMatrix
    eliminated :: EliminatedMatrix
    eliminated = {-# SCC "gaussian" #-} gaussianElimination dim buttonsMatrix (map fromIntegral target)

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

-- This is rearranged in rows to get (can also add/sub rows as needed)
-- [ 1 1 0 1 0 0 ]    [7]
-- [ 0 1 0 0 0 1 ]x = [5]
-- [ 0 0 1 1 1 0 ]    [4]
-- [ 0 0 0 0 1 1 ]    [3]

----- Gaussian Elimination -----

type Scalar = Int

type MatrixElement = Scalar

type MatrixRow = [MatrixElement]

type Matrix = [MatrixRow]

type ResultElement = Scalar

type ResultVec = [ResultElement]

type RowOffset = Int

type RowIndex = Int

type EliminatedMatrixRow = (RowOffset, MatrixRow, ResultElement)

type EliminatedMatrix = [EliminatedMatrixRow]

buttonsToMatrix :: Int -> [ButtonWiringSchematic] -> Matrix
buttonsToMatrix buttonCount = transpose . map (take buttonCount . map fromIntegral . buttonToRow 0)

buttonToRow :: Int -> [Int] -> [Int]
buttonToRow _ [] = repeat 0
buttonToRow n (x : xs)
  | n == x = 1 : buttonToRow (n + 1) xs
  | otherwise = 0 : buttonToRow (n + 1) (x : xs)

gaussianElimination :: Int -> Matrix -> ResultVec -> EliminatedMatrix
gaussianElimination dim m' r' = filter filterEndZeroSteps valsFromSteps
  where
    filterEndZeroSteps :: EliminatedMatrixRow -> Bool
    filterEndZeroSteps (_, ms, r)
      | all (== 0) ms = case r of 0 -> False; _ -> error "bad"
      | otherwise = True

    valsFromSteps :: EliminatedMatrix
    valsFromSteps = map f [0 .. (dim - 1)]
      where
        f :: RowIndex -> EliminatedMatrixRow
        f n = (\(a, b, m, r) -> (b, m !! (n - a), r !! (n - a))) $ last (filter (\(a, _, _, _) -> a <= n) steps)

    steps :: [(RowIndex, RowOffset, [[MatrixElement]], [ResultElement])]
    steps = takeWhile (\(_, _, m, _) -> not (any null m) && not (null m)) $ iterate doStep (0, 0, m', r')

    doStep :: (RowIndex, RowOffset, [[MatrixElement]], [ResultElement]) -> (RowIndex, RowOffset, [[MatrixElement]], [ResultElement])
    doStep (a, b, m, r) = case actionToTake m of
      Unconstrained -> (a, b + 1, map tail m, r)
      Continue -> (a + 1, b + 1, tail $ map tail m, tail r)
      Swap swapIndex -> (a, b, doSwap swapIndex m, doSwap swapIndex r)
      Resolve n1 n2 idx -> uncurry (a,b,,) $ doResolve n1 n2 idx m r

doSwap :: RowIndex -> [a] -> [a]
doSwap _ [] = error "Failed to swap"
doSwap n (x : xs) = (xs !! (n - 1)) : (take (n - 1) xs ++ (x : drop n xs))

doResolve :: Scalar -> Scalar -> RowIndex -> [[MatrixElement]] -> [ResultElement] -> ([[MatrixElement]], [ResultElement])
doResolve n1 n2 idx mat r
  | head row0 /= g = error "Invalid First element"
  | head rowi /= 0 = error "Row i should start with 0"
  | otherwise = (mat', r')
  where
    (g, (a, b)) = egcd n1 n2
    -- Want first row to be (a * (row 0) + b * (row idx)) (should start with `g`)
    row0 = zipWith (\r0 ri -> a * r0 + b * ri) (head mat) (mat !! idx)
    res0 = a * head r + b * (r !! idx)

    -- And then change row i to have the first digit 0
    d = (n1 + n2) `div` g
    rowi = zipWith3 (\r0 ri r0' -> (r0 + ri) - d * r0') (head mat) (mat !! idx) row0
    resi = head r + (r !! idx) - d * res0

    mat' = (element idx .~ rowi) . (element 0 .~ row0) $ mat
    r' = (element idx .~ resi) . (element 0 .~ res0) $ r

actionToTake :: Matrix -> EliminationAction
actionToTake m
  -- The first column is all zero
  | all (== 0) column = Unconstrained
  -- Top left nonzero, rest of column is zero
  | topLeft /= 0 && all (== 0) (tail column) = Continue
  -- Top left is 0, swap with first non-zero
  | topLeft == 0 = Swap firstNonZeroIndex
  -- Top left nonzero, another nonzero
  | otherwise = Resolve topLeft firstNonZero firstNonZeroIndex
  where
    column = map head m
    topLeft = head column
    firstNonZeroIndex = (+ 1) . fromJust . findIndex (/= 0) . tail $ column
    firstNonZero = column !! firstNonZeroIndex

egcd :: (Integral a, Show a) => a -> a -> (a, (a, a))
egcd x y
  | x < 0 = second (first negate) $ egcd (-x) y
  | y < 0 = second (second negate) $ egcd x (-y)
  | x < y = second swap $ egcd y x
  | yDividesX = (y, (0, 1))
  | otherwise = (g, (b', a' - d * b'))
  where
    (d, r) = x `divMod` y
    yDividesX = r == 0
    (g, (a', b')) = egcd y (x - d * y)

data EliminationAction
  = -- | This column is all zero, nothing to be done
    Unconstrained
  | -- | Swap row 0 with {0}
    Swap RowIndex
  | -- | row 0 starts with {0}; row {2} starts with {1}.
    -- | swaps, adds and multiplies as needed to get row 0 starting with gcd({0},{1}), and row {2} starting with 0.
    Resolve Scalar Scalar RowIndex
  | -- | This column starts with 1 and then is all zero
    Continue
  deriving (Show)

----- Solving from Eliminated Form -----

type SolutionElement = Int

type Solution = [SolutionElement]

data SolveStep
  = Unbounded
  | Fixed (Solution -> Maybe SolutionElement)

isUnbounded :: SolveStep -> Bool
isUnbounded Unbounded = True
isUnbounded _ = False

solveFromEliminated :: Int -> SolutionElement -> EliminatedMatrix -> Solution
solveFromEliminated dim maxResult ms =
  let !solvers = map (\(a, m, r) -> (a, getSolver m r)) ms
      !solveSteps = map (\i -> maybe Unbounded (Fixed . snd) . find ((== i) . fst) $ solvers) [0 .. dim - 1]
      !unboundedCount = length . filter isUnbounded $ solveSteps

      !unboundedAttempts = map (itemsSummingTo unboundedCount) [0 .. maxResult]
      solveAttempts = map (map (attemptSolve solveSteps)) unboundedAttempts
      firstSolve = map catMaybes solveAttempts
   in getSolution firstSolve
  where
    -- Given a partial solution with all later elements, get the element fixed by this row
    getSolver :: MatrixRow -> ResultElement -> (Solution -> Maybe SolutionElement)
    getSolver matRow !result
      | isNothing (listToMaybe matRow) = error "eliminated matrix sub row is empty"
      | listToMaybe matRow == Just 0 = error "eliminated matrix sub row starts with zero!"
      | otherwise = f
      where
        multiplier :: Scalar
        coeffs :: [Scalar]
        (!multiplier, !coeffs) = fromMaybe (error "Empty matrix row") $ uncons matRow

        getTargetValue :: Solution -> (Scalar, Scalar)
        -- getTargetValue xs | length xs + 1 /= length matRow = error "too few test values?"
        getTargetValue = (`divMod` multiplier) . (result -) . sum . zipWith (*) coeffs

        f :: Solution -> Maybe SolutionElement
        f xs = case getTargetValue xs of
          (a, 0) | a >= 0 -> Just a
          _ -> Nothing

itemsSummingTo :: Int -> Int -> [[Int]]
itemsSummingTo x _ | x < 0 = error "Negative input"
itemsSummingTo 0 _ = [[]]
itemsSummingTo 1 tot = [[tot]]
itemsSummingTo itemCount 0 = [replicate itemCount 0]
itemsSummingTo itemCount tot = concatMap (\i -> map (i :) $ itemsSummingTo (itemCount - 1) (tot - i)) [0 .. tot]

foo :: STVector s Int -> ST s (STVector s Int)
foo v = do
  v' :: STVector s Int <- MVector.generate 10 (* 10)

  x <- MVector.readMaybe v 0
  case x of
          0 -> pure ()
          x -> pure ()

  pure v'

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

-- I wish this could be better
--  - maybe we could sort the taking of `unbounded` values better?
--  - the fixed values are still potential issues though...

getSolution :: [[Solution]] -> Solution
getSolution = doIt 0 Nothing
  where
    doIt :: Int -> Maybe Solution -> [[Solution]] -> Solution
    -- We have a solution and nothing could beat it
    doIt n (Just best) _ | sum best < n = best
    -- No more of this length, move on
    doIt n best ([] : xs) = doIt (n + 1) best xs
    doIt n Nothing ((s : ss) : xs) = doIt n (Just s) (ss : xs)
    doIt n (Just sol) ((s : ss) : xs)
      | s `isBetterThan` sol = doIt n (Just s) (ss : xs)
      | otherwise = doIt n (Just sol) (ss : xs)
    doIt _ (Just best) [] = best
    doIt _ Nothing [] = error "No possible solutions"

    isBetterThan :: Solution -> Solution -> Bool
    isBetterThan a b = sum a < sum b