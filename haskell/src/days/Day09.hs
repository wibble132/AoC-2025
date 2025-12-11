module Day09 (Day09) where

import Base (Day (..), fromRight')
import Data.Bifunctor (bimap)
import Data.Char (digitToInt)
import Data.List (sort, sortOn, elemIndex)
import Data.Maybe (fromJust, listToMaybe)
import Text.Parsec (Parsec, char, digit, many, parse)

data Day09

instance Day Day09 where
  type ParsedData Day09 = Data

  dayNumber :: Int
  dayNumber = 9
  parseInput :: String -> Data
  parseInput = fromRight' . parse doParse ""

  part1 :: Data -> Integer
  part1 = toInteger . doPart1
  part2 :: Data -> Integer
  part2 = toInteger . doPart2

-- ### Parsing ###

type Pos = (Int, Int)

newtype Data = Data [Pos]

doParse :: Parsec String () Data
doParse = Data <$> many (number >>= (\n1 -> char ',' >> number >>= (\n2 -> (n1, n2) <$ char '\n')))

number :: Parsec String () Int
number = foldl' (\acc c -> acc * 10 + digitToInt c) 0 <$> many digit

-- ### Part 1 ###

doPart1 :: Data -> Int
doPart1 (Data d) = maximum . map rectSize . rects $ d

pairs :: [Pos] -> [(Pos, Pos)]
pairs [] = []
pairs (p : ps) = map (p,) ps ++ pairs ps

rectSize :: Rect -> Int
rectSize r = (dx + 1) * (dy + 1)
  where
    dist a b = abs (a - b)
    dx = dist (rMinX r) (rMaxX r)
    dy = dist (rMinY r) (rMaxY r)

-- ### Part 2 ###

data Rect = Rect {rMinX :: Int, rMaxX :: Int, rMinY :: Int, rMaxY :: Int} deriving Show

mkRect :: Pos -> Pos -> Rect
mkRect (x1, y1) (x2, y2) = Rect (min x1 x2) (max x1 x2) (min y1 y2) (max y1 y2)

rects :: [Pos] -> [Rect]
rects = map (uncurry mkRect) . pairs

data PosCompressList = PCL [Int] [Int]

compressPoints :: [Pos] -> (PosCompressList, [Pos])
compressPoints ps = (PCL xs ys, idxs)
  where
    xs = sort $ map fst ps
    ys = sort $ map snd ps

    lookupP :: Pos -> Pos
    lookupP = bimap (fromJust . (`elemIndex` xs)) (fromJust . (`elemIndex` ys))
    idxs = map lookupP ps

rectSizeCompressed :: PosCompressList -> Rect -> Int
rectSizeCompressed (PCL xs ys) rect = rectSize (Rect xmin xmax ymin ymax)
  where
    xmin = xs !! rMinX rect
    xmax = xs !! rMaxX rect
    ymin = ys !! rMinY rect
    ymax = ys !! rMaxY rect

doPart2 :: Data -> Int
doPart2 (Data d) = rectSizeCompressed pcl . fromJust . listToMaybe $ filteredRects
  where
    (pcl, cps) = compressPoints d
    compressedRects = rects cps
    sortedRects = sortOn (negate . rectSizeCompressed pcl) compressedRects
    filteredRects = filter (rectInPoly (polyEdges cps)) sortedRects

rectPoints :: Rect -> [Pos]
rectPoints r = concatMap (\y -> map (,y) [rMinX r .. rMaxX r]) [rMinY r .. rMaxY r]

rectInPoly :: [(Pos, Pos)] -> Rect -> Bool
rectInPoly edges rect = all (isInPolygon edges) (rectPoints rect)

isInPolygon :: [(Pos, Pos)] -> Pos -> Bool
isInPolygon edges (x, y) = checkLeft edges 0
  where
    checkLeft :: [(Pos, Pos)] -> Int -> Bool
    checkLeft [] n = n `mod` 2 == 1
    checkLeft (((x1, y1), (x2, y2)) : es) n
      -- Edges are included, let's include them!
      | isOnHorizontalLine = True
      | isOnVerticalLine = True
      -- This edge is not left of this point, move on to other edges
      | isHorizonalLine = checkLeft es n
      | not isYRange = checkLeft es n
      | not isLeft = checkLeft es n
      -- This must be a vertical edge to the left of this point - count it.
      | otherwise = checkLeft es (n + 1)
      where
        isOnHorizontalLine = (y1 == y2) && (y1 == y) && ((min x1 x2) <= x && x <= (max x1 x2))
        isOnVerticalLine = (x1 == x2) && (x1 == x) && ((min y1 y2) <= y && y <= (max y1 y2))

        isHorizonalLine = (y1 == y2)
        isYRange = ((min y1 y2) < y && y <= (max y1 y2))
        isLeft = x1 <= x && x2 <= x

polyEdges :: [Pos] -> [(Pos, Pos)]
polyEdges ps'@(p' : _) = doIt p' ps'
  where
    doIt :: Pos -> [Pos] -> [(Pos, Pos)]
    doIt p (p1 : ps@(p2 : _)) = (p1, p2) : doIt p ps
    doIt p (p1 : []) = [(p1, p)]
    doIt _ [] = error "No more points in poly"
polyEdges [] = error "No points in poly"

