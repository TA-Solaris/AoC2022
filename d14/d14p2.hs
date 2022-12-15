
import System.IO  
import Control.Monad

import Data.List.Split (splitOn)

import Data.Map (Map, empty, insert, filterWithKey, keys)
import qualified Data.Map (filter, lookup)
import Data.Maybe (isNothing)

-- Defining types
type Position = (Int, Int)
type Path = [Position]

data Block = Rock | Sand
    deriving (Eq, Show, Read)
type World = Map Position Block

-- Translating Input
translateInput :: String -> World
translateInput str = translateToWorld $ mapPaths $ translateStr str
    where
        translateStr :: String -> [Path]
        translateStr str = [[(\[x, y] -> (read x, read y)) $ splitOn "," pos | pos <- splitOn " -> " line] | line <- lines str]
        mapPaths :: [Path] -> [Position]
        mapPaths [] = []
        mapPaths (x:xs) = pathToPostitions x ++ mapPaths xs
        pathToPostitions :: Path -> [Position]
        pathToPostitions [] = []
        pathToPostitions path = concat [[(x, y) | x <- [min x1 x2 .. max x1 x2], y <- [min y1 y2 .. max y1 y2]] | ((x1, y1), (x2, y2)) <- zip path (tail path)]
        translateToWorld :: [Position] -> World
        translateToWorld [] = empty
        translateToWorld (x:xs) = insert x Rock $ translateToWorld xs

-- Simulating Sand
findNextFall :: Position -> World -> Maybe Position
findNextFall pos world | null (getPositionsBelow pos world) = Nothing
                       | otherwise = Just (minimum $ getPositionsBelow pos world)
    where
        getPositionsBelow :: Position -> World -> [Position]
        getPositionsBelow (px, py) world = keys $ filterWithKey (\(mx, my) _ -> mx == px && my > py) world

placeSand :: Position -> World -> Int -> World
placeSand pos world voidBorder | isNothing (findNextFall pos world) = insert (fst pos, voidBorder - 1) Sand world -- Abyss case
                               | isNothing (Data.Map.lookup ((\(Just (x, y)) -> (x - 1, y)) $ findNextFall pos world) world) = placeSand ((\(Just (x, y)) -> (x - 1, y - 1)) $ findNextFall pos world) world voidBorder -- Left
                               | isNothing (Data.Map.lookup ((\(Just (x, y)) -> (x + 1, y)) $ findNextFall pos world) world) = placeSand ((\(Just (x, y)) -> (x + 1, y - 1)) $ findNextFall pos world) world voidBorder -- Right
                               | otherwise = insert ((\(Just (x, y)) -> (x, y - 1)) $ findNextFall pos world) Sand world -- Placement

simulateSand :: World -> Int -> World
simulateSand world voidBorder | placeSand (500, 0) world voidBorder == world = world
                              | otherwise = simulateSand (placeSand (500, 0) world voidBorder) voidBorder

-- Parts
howMuchSand :: World -> Int
howMuchSand world = length $ Data.Map.filter (== Sand) world

getVoidBorder :: World -> Int
getVoidBorder world = (+2) $ maximum $ map snd $ keys world

main = do
        handle <- openFile "d14/d14.txt" ReadMode
        contents <- hGetContents handle
        print $ howMuchSand $ simulateSand (translateInput contents) (getVoidBorder $ translateInput contents)
        hClose handle

-- This implementation is too slow for it to run a big input
