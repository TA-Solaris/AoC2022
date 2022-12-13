
import System.IO  
import Control.Monad

import Data.List (findIndex, minimumBy, elemIndex)

import Prelude hiding (null, lookup)

import Data.Set (Set, singleton, null, toList, delete)
import qualified Data.Set (insert)
import Data.Map (Map, empty, insert, lookup, member)

-- Defining Types
type Position = (Int, Int)
type Grid = [[Int]]

-- Translating Input
translateInput :: String -> (Grid, Position, Position)
translateInput [] = error "Cannot have empty input [Error 2]"
translateInput str = (getGrid str, getStart str, getEnd str)

getGrid :: String -> Grid
getGrid [] = error "Cannot have empty input (grid) [Error 3]"
getGrid str = [[getHeight c | c <- line] | line <- lines str]

getStart :: String -> Position
getStart str = ((\(Just a) -> a) $ elemIndex 'S' $ lines str !! (\(Just a) -> a) (findIndex (/=Nothing) [elemIndex 'S' line | line <- lines str]), (\(Just a) -> a) $ findIndex (/=Nothing) [elemIndex 'S' line | line <- lines str])

getEnd :: String -> Position
getEnd str = ((\(Just a) -> a) $ elemIndex 'E' $ lines str !! (\(Just a) -> a) (findIndex (/=Nothing) [elemIndex 'E' line | line <- lines str]), (\(Just a) -> a) $ findIndex (/=Nothing) [elemIndex 'E' line | line <- lines str])

getHeight :: Char -> Int
getHeight 'S' = 1
getHeight 'E' = 26
getHeight x | x `elemIndex` ['a' .. 'z'] == Nothing = error "Invalid character in input [Error 1]"
            | otherwise = (+1) $ (\(Just a) -> a) $ x `elemIndex` ['a' .. 'z']

-- Defining helper functions
absDiff :: Num a => a -> a -> a
absDiff n = abs . (n-)

infinityMap :: Grid -> Map Position Int
infinityMap grid = insertAll [((x, y), maxBound::Int) | x <- [0 .. length (head grid)], y <- [0 .. length grid]] empty
-- read "Infinity" (possible way of getting it to work better but I cba to get it to work)

insertAll :: Ord k => [(k, a)] -> Map k a -> Map k a
insertAll [] myMap = myMap
insertAll ((k, a):rest) myMap = insertAll rest (insert k a myMap)

goodNeighbor :: Position -> Position -> Grid -> Bool
goodNeighbor (cx, cy) (nx, ny) grid = nx >= 0 && nx < length (head grid) && cx >= 0 && cx < length (head grid) && ny >= 0 && ny < length grid && cy >= 0 && cy < length grid

getNeighbors :: Position -> Grid -> [Position]
getNeighbors pos grid = [(nx, ny) | (nx, ny) <- [(fst pos + x, snd pos + y) | (x, y) <- [(-1, 0), (1, 0), (0, -1), (0, 1)], goodNeighbor pos (fst pos + x, snd pos + y) grid], (grid !! ny) !! nx <= (grid !! snd pos) !! fst pos + 1]

getCurrent :: Set Position -> Map Position Int -> Position
getCurrent openSet fScore = fst $ minimumBy comp $ [(pos, lookup pos fScore) | pos <- toList openSet]
    where
        comp (_, Nothing) (_, _) = error "Lookup Failed [Error 5]"
        comp (_, _) (_, Nothing) = error "Lookup Failed [Error 5]"
        comp (_, Just a) (_, Just b) = compare a b

handleNeighbors :: Position -> [Position] -> (Position -> Int) -> Set Position -> Map Position Position -> Map Position Int -> Map Position Int -> (Set Position, Map Position Position, Map Position Int, Map Position Int)
handleNeighbors _ [] _ openSet cameFrom gScore fScore = (openSet, cameFrom, gScore, fScore)
handleNeighbors current (neighbor:neighbors) h openSet cameFrom gScore fScore | (\(Just a) -> a) (lookup current gScore) + 1 < (\(Just a) -> a) (lookup neighbor gScore) = handleNeighbors current neighbors h (Data.Set.insert neighbor openSet) (insert neighbor current cameFrom) (insert neighbor ((\(Just a) -> a) (lookup current gScore) + 1) gScore) (insert neighbor ((\(Just a) -> a) (lookup current gScore) + 1 + h neighbor) fScore)
                                                                              | otherwise = handleNeighbors current neighbors h openSet cameFrom gScore fScore

-- (For the A* heuristic I'm just getting the manhattan distance, possibly could do something with height too if I want)
heuristic :: Position -> Position -> Int
heuristic (ex, ey) (nx, ny) = absDiff ex nx + absDiff ey ny

reconstructPath :: Map Position Position -> Position -> [Position]
reconstructPath cameFrom pos | member pos cameFrom = pos : reconstructPath cameFrom ((\(Just a) -> a) $ lookup pos cameFrom)
                             | otherwise = [pos]

aStar :: Grid -> Position -> Position -> [Position]
aStar grid start end = rec_a_star grid end (heuristic end) (singleton start) empty (insert start 0 $ infinityMap grid) (insert start (heuristic end start) $ infinityMap grid)
    where
        rec_a_star :: Grid -> Position -> (Position -> Int) -> Set Position -> Map Position Position -> Map Position Int -> Map Position Int -> [Position]
        rec_a_star grid goal h openSet cameFrom gScore fScore | null openSet = error "A* Search Failed [Error 4]" -- Fail case
                                                              | getCurrent openSet fScore == goal = reverse $ reconstructPath cameFrom goal -- Success case
                                                              | otherwise = (\(openSet', cameFrom', gScore', fScore') -> rec_a_star grid goal h openSet' cameFrom' gScore' fScore') $ handleNeighbors (getCurrent openSet fScore) (getNeighbors (getCurrent openSet fScore) grid) h (delete (getCurrent openSet fScore) openSet) cameFrom gScore fScore

-- Parts
part1 :: (Grid, Position, Position) -> Int
part1 (grid, start, end) = length (aStar grid start end) - 1

main = do
        handle <- openFile "d12/d12.txt" ReadMode
        contents <- hGetContents handle
        print $ part1 $ translateInput contents
        hClose handle
