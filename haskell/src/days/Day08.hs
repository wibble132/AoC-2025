module Day08(Day08) where

import Data.Char (digitToInt)
import Data.List (sortOn, sort, nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, listToMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

import Text.Parsec (Parsec, parse, sepBy, newline, digit, many, char)

import Base (Day (..), fromRight')

data Day08

instance Day Day08 where
  type ParsedData Day08 = Data

  dayNumber :: Int
  dayNumber = 8
  parseInput :: String -> Data
  parseInput = fromRight' . parse doParse ""

  part1 :: Data -> Integer
  part1 = toInteger . doPart1
  part2 :: Data -> Integer
  part2 = toInteger . doPart2


-- ### Parsing ###

type Point = (Integer, Integer, Integer)
newtype Data = Data [Point]

doParse :: Parsec String () Data
doParse = Data <$> parseLine `sepBy` newline

parseLine :: Parsec String () Point
parseLine = do
    ds1 <- many digit
    _ <- char ','
    ds2 <- many digit
    _ <- char ','
    ds3 <- many digit
    pure (digitsToNum ds1, digitsToNum ds2,digitsToNum ds3)

digitsToNum :: [Char] -> Integer
digitsToNum = foldl' (\acc c -> acc * 10 + (toInteger . digitToInt) c) 0


-- ### Part 1 ###

doPart1 :: Data -> Int
doPart1 (Data d) = product . map length . largestNGroups 3 $ connected
  where
    sortedPairs :: [(Point, Point)]
    sortedPairs = sortOn (uncurry sqrDistance) . pairs $ d

    emptyMap :: Map Point [Point]
    emptyMap = Map.fromList . map (\x -> (x, [x])) $ d

    tryConnect :: (Point, Point) -> Map Point [Point] -> Maybe (Map Point [Point])
    tryConnect _connection@(p1, p2) existingMap
      -- | traceShow ("Connecting ", (p1, p2)) False = undefined
      | p2 `elem` (existingMap Map.! p1) = Nothing
      | otherwise = Just result
      where
        result = foldl' (\acc p -> Map.insert p newGroup acc) existingMap newGroup
        p1Connections = existingMap Map.! p1
        p2Connections = existingMap Map.! p2
        newGroup = nub (p1Connections ++ p2Connections)
    
    connectN :: Int -> [(Point, Point)] -> Map Point [Point] -> Map Point [Point]
    connectN 0 _ m = m
    connectN n (p:ps) m = case tryConnect p m of
        Nothing -> connectN (n - 1) ps m
        Just m' -> connectN (n - 1) ps m'
    connectN _ [] _ = error "No more connections to make!"

    connected :: Map Point [Point]
    connected = connectN 1000 sortedPairs emptyMap

    largestNGroups :: Int -> Map Point [Point] -> [[Point]]
    largestNGroups n = take n . nub . map sort . sortOn (negate . length) . Map.elems


sqrDistance :: Point -> Point -> Integer
sqrDistance (x1, y1, z1) (x2, y2, z2) = (x1 - x2) ^ two + (y1 - y2) ^ two + (z1 - z2) ^ two
  where
    two = 2 :: Int

pairs :: [Point] -> [(Point, Point)]
pairs [] = []
pairs (p : ps) = map (p,) ps ++ pairs ps

-- ### Part 2 ###


doPart2 :: Data -> Integer
doPart2 (Data d) = (\((a,_,_), (b,_,_)) -> a * b) getLastConnection
  where
    sortedPairs :: [(Point, Point)]
    sortedPairs = sortOn (uncurry sqrDistance) . pairs $ d

    emptyMap :: Map Point (Set Point)
    emptyMap = Map.fromList . map (\x -> (x, Set.singleton x)) $ d

    tryConnect :: (Point, Point) -> Map Point (Set Point) -> Maybe (Map Point (Set Point))
    tryConnect _connection@(p1, p2) existingMap
      -- | traceShow ("Connecting ", (p1, p2)) False = undefined
      | p2 `elem` (existingMap Map.! p1) = Nothing
      | otherwise = Just result
      where
        result = foldl' (\acc p -> Map.insert p newGroup acc) existingMap newGroup
        p1Connections = existingMap Map.! p1
        p2Connections = existingMap Map.! p2
        newGroup :: Set Point
        newGroup = p1Connections <> p2Connections
    
    connectWhile :: (Map Point (Set Point) -> Bool) -> [(Point, Point)] -> Map Point (Set Point) -> ([(Point, Point)], Map Point (Set Point))
    connectWhile f ps m | not (f m) = (ps, m)
    connectWhile f (p:ps) m = case tryConnect p m of
        Nothing -> connectWhile f ps m
        Just m' -> connectWhile f ps m'
    connectWhile _ [] _ = error "No more connections to make!"

    connected :: ([(Point, Point)], Map Point (Set Point))
    connected = connectWhile f sortedPairs emptyMap
      where
        f :: Map Point (Set Point) -> Bool
        f = (< length d) . Set.size . snd . fromJust . Map.lookupMin

    getLastConnection :: (Point, Point)
    getLastConnection = doIt sortedPairs
      where
        oneAfter :: (Point, Point)
        oneAfter = fromJust . listToMaybe . fst $ connected

        doIt :: [(Point, Point)] -> (Point, Point)
        doIt (p1 : p2 : ps)
          | p2 == oneAfter = p1
          | otherwise = doIt (p2 : ps)
        doIt _ = error "Cannot find item"
