module Base (Day(..), readInputFile, fromRight') where

class Day a where
    type ParsedData a

    dayNumber :: Int
    parseInput :: String -> ParsedData a
    part1 :: ParsedData a -> Integer
    part2 :: ParsedData a -> Integer 

readInputFile :: forall a. (Day a) => IO String
readInputFile = readFile fileName
    where
        n = dayNumber @a
        nStr = show n

        pad :: String -> String
        pad [c] = ['0', c]
        pad cs = cs

        fileName :: FilePath
        fileName = "../inputs/day" ++ pad nStr ++ ".txt"


fromRight' :: Show a => Either a b -> b
fromRight' (Right x) = x
fromRight' (Left x) = error $ "Failed on fromRight' with " ++ show x