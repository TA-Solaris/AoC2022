
import System.IO  
import Control.Monad

import Data.List.Split (splitOn)

-- Defining types
type Position = (Int, Int)
type Path = [Position]

data Block = Rock | Sand | Air
    deriving (Eq, Show, Read)
type World = [[Block]]

-- Translating Input
translateInput :: String -> World
translateInput str = translateToWorld (mapPaths $ translateStr str) (getWorld $ mapPaths $ translateStr str)
    where
        translateStr :: String -> [Path]
        translateStr str = [[(\[x, y] -> (read x, read y)) $ splitOn "," pos | pos <- splitOn " -> " line] | line <- lines str]
        mapPaths :: [Path] -> [Position]
        mapPaths [] = []
        mapPaths (x:xs) = pathToPostitions x ++ mapPaths xs
        pathToPostitions :: Path -> [Position]
        pathToPostitions [] = []
        pathToPostitions path = concat [[(x, y) | x <- [min x1 x2 .. max x1 x2], y <- [min y1 y2 .. max y1 y2]] | ((x1, y1), (x2, y2)) <- zip path (tail path)]
        -- I am abandoning space complexity, I want to no longer spend 32270 seconds running my code
        getWorld :: [Position] -> World
        getWorld positions = [[if y == maximum (map snd positions) + 2 then Rock else Air | _ <- [0 .. maximum (map fst positions) * 2]] | y <- [0 .. maximum (map snd positions) + 2]]
        translateToWorld :: [Position] -> World -> World
        translateToWorld [] world = world
        translateToWorld ((x, y):xs) world = translateToWorld xs (nnreplace x y Rock world)

nreplace :: Int -> a -> [a] -> [a]
nreplace _ _ [] = []
nreplace n v (x:xs) | n == 0 = v:xs
                    | otherwise = x:nreplace (n-1) v xs

nnreplace :: Int -> Int -> a -> [[a]] -> [[a]]
nnreplace x y v w = nreplace y (nreplace x v (w !! y)) w

-- Simulating Sand
placeSand :: Position -> World -> World
placeSand (x, y) world | (world !! (y + 1)) !! x == Air = placeSand (x, y + 1) world -- Bottom free
                       | (world !! (y + 1)) !! (x - 1) == Air = placeSand (x - 1, y + 1) world -- Left free
                       | (world !! (y + 1)) !! (x + 1) == Air = placeSand (x + 1, y + 1) world -- Right free
                       | (world !! y) !! x == Air = nnreplace x y Sand world -- Place Sand
                       | otherwise = world -- End case

simulateSand :: World -> World
simulateSand world | placeSand (500, 0) world == world = world
                   | otherwise = simulateSand (placeSand (500, 0) world)

-- Parts
howMuchSand :: World -> Int
howMuchSand world = length $ filter (== Sand) $ concat world

-- Printing
sequenceIO_ :: [IO a] -> IO ()
sequenceIO_ [] = return ()
sequenceIO_ (x:xs) = do {x; sequenceIO_ xs}

ppWorld :: World -> IO ()
ppWorld world = sequenceIO_ [sequenceIO_ (putChar '\n':[printer | block <- row, (blockType, printer) <- [(Rock, putChar '#'), (Sand, putChar 'o'), (Air, putChar '.')], blockType == block]) | row <- world]

main = do
        handle <- openFile "d14/d14.txt" ReadMode
        contents <- hGetContents handle
        print $ howMuchSand $ simulateSand $ translateInput contents
        hClose handle
