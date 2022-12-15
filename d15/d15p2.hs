
import System.IO  
import Control.Monad

import Data.List.Split (splitOn)
import Data.Text (replace, pack, unpack)

import Data.Maybe (isJust, isNothing)
import Data.List (sort)

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

getRanges :: [SensorBeacon] -> Int -> [Range]
getRanges xs y = combineRanges $ sort $ allRanges xs y
    where
        allRanges :: [SensorBeacon] -> Int -> [Range]
        allRanges xs y = [(sx - manhattan (sx, sy) (bx, by) + absDiff sy y, sx + manhattan (sx, sy) (bx, by) - absDiff sy y) | ((sx, sy), (bx, by)) <- xs, sy + manhattan (sx, sy) (bx, by) >= y && sy - manhattan (sx, sy) (bx, by) <= y]
        combineRange :: Range -> Range -> Maybe Range
        combineRange (la, ua) (lb, ub) | (la <= lb && ua <= ub && ua >= lb) || (lb >= la && lb <= ua && ub >= ua) = Just (la, ub) -- Lower Cases
                                       | (lb <= la && ub <= ua && ub >= la) || (la >= lb && la <= ub && ua >= ub) = Just (lb, ua) -- Lower Cases
                                       | la >= lb && ua <= ub = Just (lb, ub) -- Subsuming case
                                       | lb >= la && ub <= ua = Just (la, ua) -- Subsuming case
                                       | otherwise = Nothing
        -- Probs very inefficient
        combineRanges :: [Range] -> [Range]
        combineRanges (range1:range2:ranges) | null [combineRange a b | a <- range1:range2:ranges, b <- range1:range2:ranges, a /= b, isJust $ combineRange a b] = range1:range2:ranges -- Base case
                                             | isNothing (combineRange range1 range2) = combineRanges (range1:combineRanges (range2:ranges))
                                             | otherwise = combineRanges ((\(Just a) -> a) (combineRange range1 range2):ranges)
        combineRanges range = range

inRange :: Range -> Range -> Bool
(la, ua) `inRange` (lb, ub) = la >= lb && ua <= ub

findy :: [SensorBeacon] -> Int -> Int -> Int
findy xs y ymax | y > ymax = error "None Found"
                | any ((0, ymax) `inRange`) (getRanges xs y) = findy xs (y + 1) ymax
                | otherwise = y

findx :: [Range] -> Int -> Int
findx ((_, upper):xs) maxrange | upper >= 0 && upper < maxrange = upper + 1
                               | otherwise = findx xs maxrange
findx _ _ = error "Should not happen"

getPart2 :: [SensorBeacon] -> Int -> Int
getPart2 sb maxrange = 4000000 * findx (getRanges sb $ findy sb 0 maxrange) maxrange + findy sb 0 maxrange

main = do
        handle <- openFile "d15/d15.txt" ReadMode
        contents <- hGetContents handle
        print $ getPart2 (translateInput contents) 20 -- Change this number depending on if it is the example input or an actual input
        hClose handle
