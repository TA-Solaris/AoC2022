
import System.IO  
import Control.Monad

import Data.List.Split (splitOn)

import Data.Map (Map, empty, insert, filterWithKey, keys)
import qualified Data.Map (filter, lookup)
import Data.Maybe (isNothing)

-- Functional parsing library from chapter 13 of Programming in Haskell,
-- Graham Hutton, Cambridge University Press, 2016.
--import Parsing

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

-- Parsing

-- Doesn't work rn, will try cbl

--extractParsed :: [(a, String)] -> a
--extractParsed [(a, "")] = a
--extractParsed _ = error "Passing failed or multiple parses generated [Error 1]"

-- Defines language:
-- Int,Int -> Int,Int -> Int,Int -> Int,Int
-- Int,Int -> Int,Int
-- ...

--parsePaths :: Parser [Path]
--parsePaths = do p <- parsePath
--                ps <- many (do symbol "\n"
--                               parsePath)
--                return (p:ps)

--parsePath :: Parser Path
--parsePath = do p <- parsePosition
--               ps <- many (do symbol " -> "
--                              parsePosition)
--               return (p:ps)

--parsePosition :: Parser Position
--parsePosition = do x <- int
--                   symbol ","
--                   y <- int
--                   return (x, y)

-- Simulating Sand
placeSand :: Position -> World -> World
placeSand pos world | isNothing (findNextFall pos world) = world -- Abyss case
                    | isNothing (Data.Map.lookup ((\(Just (x, y)) -> (x - 1, y)) $ findNextFall pos world) world) = placeSand ((\(Just (x, y)) -> (x - 1, y - 1)) $ findNextFall pos world) world -- Left
                    | isNothing (Data.Map.lookup ((\(Just (x, y)) -> (x + 1, y)) $ findNextFall pos world) world) = placeSand ((\(Just (x, y)) -> (x + 1, y - 1)) $ findNextFall pos world) world -- Right
                    | otherwise = insert ((\(Just (x, y)) -> (x, y - 1)) $ findNextFall pos world) Sand world -- Placement

findNextFall :: Position -> World -> Maybe Position
findNextFall pos world | null (getPositionsBelow pos world) = Nothing
                       | otherwise = Just (minimum $ getPositionsBelow pos world)
    where
        getPositionsBelow :: Position -> World -> [Position]
        getPositionsBelow (px, py) world = keys $ filterWithKey (\(mx, my) _ -> mx == px && my > py) world

simulateSand :: World -> World
simulateSand world | placeSand (500, 0) world == world = world
                   | otherwise = simulateSand (placeSand (500, 0) world)

-- Parts
howMuchSand :: World -> Int
howMuchSand world = length $ Data.Map.filter (== Sand) world

main = do
        handle <- openFile "d14/d14.txt" ReadMode
        contents <- hGetContents handle
        print $ howMuchSand $ simulateSand $ translateInput contents
        hClose handle
