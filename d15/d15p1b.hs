
import System.IO  
import Control.Monad

import Data.List.Split (splitOn)
import Data.Text (replace, pack, unpack)

-- Defining Data Types
type SensorBeacon = ((Int, Int), (Int, Int))
type Range = (Int, Int)

-- Translating Input
translateInput :: String -> [SensorBeacon]
translateInput [] = []
translateInput str = [(\[s, b] -> ((\[x, y] -> (read x, read y)) $ splitOn ", y=" s, (\[x, y] -> (read x, read y)) $ splitOn ", y=" b)) $ splitOn ": closest beacon is at x=" line | line <- lines $ unpack $ replace (pack "Sensor at x=") (pack "") (pack str)]

-- Defining helper functions
absDiff :: Num a => a -> a -> a
absDiff n = abs . (n-)

manhattan :: (Int, Int) -> (Int, Int) -> Int
manhattan (sx, sy) (bx, by) = absDiff sx bx + absDiff sy by

getEdges :: [SensorBeacon] -> Range
getEdges xs = (minimum (map (fst . fst) xs) - maximum (map (uncurry manhattan) xs), maximum (map (fst . fst) xs) + maximum (map (uncurry manhattan) xs))

inRange :: (Int, Int) -> [SensorBeacon] -> Bool
inRange pos xs = not $ null [(spos, bpos) | (spos, bpos) <- xs, manhattan spos pos <= manhattan spos bpos, pos /= bpos]

getCount :: [SensorBeacon] -> Range -> Int -> Int
getCount xs (current, end) y | current > end = 0
                             | inRange (current, y) xs = 1 + getCount xs (current + 1, end) y
                             | otherwise = getCount xs (current + 1, end) y

main = do
        handle <- openFile "d15/d15.txt" ReadMode
        contents <- hGetContents handle
        print $ getCount (translateInput contents) (getEdges $ translateInput contents) 2000000
        hClose handle

-- Works but there should be a better way to do this (probs ranges, see part 2)
