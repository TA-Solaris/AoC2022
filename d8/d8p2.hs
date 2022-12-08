
import System.IO  
import Control.Monad

import Data.List (transpose)

-- Defining Types
data Visability = Visible | Invisible
    deriving (Show, Read, Eq)

data Direction = U | D | L | R
    deriving (Show, Read, Eq)

-- For turning the input into a grid of int
translateInput :: String -> [[(Int, Visability)]]
translateInput [] = []
translateInput str = [[(read (x:""), Invisible) | x <- line] | line <- lines str]

translateInput' :: String -> [[Int]]
translateInput' [] = []
translateInput' str = [[read (x:"") | x <- line] | line <- lines str]

-- Helper Functions
nreplace :: Int -> a -> [a] -> [a]
nreplace _ _ [] = []
nreplace n v (x:xs) | n == 0 = v:xs
                    | otherwise = x:nreplace (n-1) v xs

mark :: Int -> Int -> [[(Int,Visability)]] -> Visability -> [[(Int,Visability)]]
mark _ _ [] _ = []
mark x y grid new | 0 <= x && x <= (length (grid !! 0) - 1) && 0 <= y && y <= (length grid - 1) = nreplace y (nreplace x (fst ((grid !! y) !! x), new) (grid !! y)) grid
                  | otherwise = grid

countVisible :: [[(Int,Visability)]] -> Int
countVisible grid = length $ filter (== Visible) $ map snd $ foldr (++) [] (grid)

markEdges :: [[(Int,Visability)]] -> [[(Int,Visability)]]
markEdges grid = transpose $ markBottom $ markTop $ transpose $ markBottom $ markTop grid
    where
        markTop,markBottom :: [[(Int,Visability)]] -> [[(Int,Visability)]]
        markTop grid = nreplace 0 (map (\(n, _) -> (n, Visible)) (grid !! 0)) grid
        markBottom grid = nreplace (length grid - 1) (map (\(n, _) -> (n, Visible)) (grid !! (length grid - 1))) grid
        -- I cba to write mark left and mark right

markDirection :: Int -> Int -> [[(Int,Visability)]] -> Direction -> Int -> [[(Int,Visability)]]
markDirection x y grid dir last | x < 0 || x >= length (grid !! 0) || y < 0 || y >= length grid || (dir == L && x <= 0) || (dir == R && x >= length (grid !! 0) - 1) || (dir == U && y <= 0) || (dir == D && y >= length grid - 1) = grid -- Protecting against index errors
                                | dir == U && last < fst ((grid !! (y - 1)) !! x) = markDirection x (y - 1) (mark x (y - 1) grid Visible) dir (fst ((grid !! (y - 1)) !! x))
                                | dir == U = markDirection x (y - 1) grid dir last
                                | dir == D && last < fst ((grid !! (y + 1)) !! x) = markDirection x (y + 1) (mark x (y + 1) grid Visible) dir (fst ((grid !! (y + 1)) !! x))
                                | dir == D = markDirection x (y + 1) grid dir last
                                | dir == L && last < fst ((grid !! y) !! (x - 1)) = markDirection (x - 1) y (mark (x - 1) y grid Visible) dir (fst ((grid !! y) !! (x - 1)))
                                | dir == L = markDirection (x - 1) y grid dir last
                                | dir == R && last < fst ((grid !! y) !! (x + 1)) = markDirection (x + 1) y (mark (x + 1) y grid Visible) dir (fst ((grid !! y) !! (x + 1)))
                                | dir == R = markDirection (x + 1) y grid dir last
                                | otherwise = grid

lookIn :: [[(Int,Visability)]] -> [[(Int,Visability)]]
lookIn grid = lookRight (lookLeft (lookBottom (lookTop grid 0) 0) 0) 0

lookTop,lookLeft,lookRight,lookBottom :: [[(Int,Visability)]] -> Int -> [[(Int,Visability)]]
lookTop grid x | x >= length (grid !! 0) = grid
               | otherwise = lookTop (markDirection x 0 grid D (fst ((grid !! 0) !! x))) (x + 1)
lookBottom grid x | x >= length (grid !! 0) = grid
                  | otherwise = lookBottom (markDirection x (length grid - 1) grid U (fst ((grid !! (length grid - 1)) !! x))) (x + 1)
lookLeft grid y | y >= length grid = grid
                | otherwise = lookLeft (markDirection 0 y grid R (fst ((grid !! y) !! 0))) (y + 1)
lookRight grid y | y >= length grid = grid
                 | otherwise = lookRight (markDirection (length (grid !! 0) - 1) y grid L (fst ((grid !! y) !! (length (grid !! 0) - 1)))) (y + 1)

scan :: Int -> [Int] -> Int
scan _ [] = 0
scan tree (x:xs) | x >= tree = 1
                 | otherwise = 1 + scan tree xs

allDirections :: Int -> Int -> [[Int]] -> [Int]
allDirections x y grid = map (scan ((grid !! y) !! x)) $ getLines x y grid

getLines :: Int -> Int -> [[Int]] -> [[Int]]
getLines x y grid = ((\(l, r) -> [(reverse l), (drop 1 r)]) (splitAt x (grid !! y))) ++ ((\(l, r) -> [(reverse l), (drop 1 r)]) (splitAt y ((transpose grid) !! x)))

-- Tests
test :: IO ()
test = do
        putStr "\n--- RUNNING TESTS ---\n"

        putStr "\n-Checking MarkDirection Works\n"
        putStr "MarkLeft:"
        if markDirection 4 0 [[(5,Invisible),(1,Invisible),(2,Invisible),(2,Invisible),(1,Invisible)]] L 1 == [[(5,Visible),(1,Invisible),(2,Invisible),(2,Visible),(1,Invisible)]] then
            putStr "Passed\n"
        else
            do
                putStr "Failed\n"
                print $ markDirection 4 0 [[(5,Invisible),(1,Invisible),(2,Invisible),(2,Invisible),(1,Invisible)]] L 1
        putStr "MarkRight:"
        if markDirection 0 0 [[(0,Invisible),(3,Invisible),(2,Invisible),(3,Invisible),(4,Invisible)]] R 0 == [[(0,Invisible),(3,Visible),(2,Invisible),(3,Invisible),(4,Visible)]] then
            putStr "Passed\n"
        else
            do
                putStr "Failed\n"
                print $ markDirection 0 0 [[(0,Invisible),(3,Invisible),(2,Invisible),(3,Invisible),(4,Invisible)]] R 0
        putStr "MarkUp:"
        if markDirection 0 4 [[(5,Invisible)],[(3,Invisible)],[(2,Invisible)],[(3,Invisible)],[(0,Invisible)]] U 0 == [[(5,Visible)],[(3,Invisible)],[(2,Invisible)],[(3,Visible)],[(0,Invisible)]] then
            putStr "Passed\n"
        else
            do
                putStr "Failed\n"
                print $ markDirection 0 4 [[(5,Invisible)],[(3,Invisible)],[(2,Invisible)],[(3,Invisible)],[(0,Invisible)]] U 0
        putStr "MarkDown:"
        if markDirection 0 0 [[(0,Invisible)],[(3,Invisible)],[(2,Invisible)],[(3,Invisible)],[(4,Invisible)]] D 0 == [[(0,Invisible)],[(3,Visible)],[(2,Invisible)],[(3,Invisible)],[(4,Visible)]] then
            putStr "Passed\n"
        else
            do
                putStr "Failed\n"
                print $ markDirection 0 4 [[(0,Invisible)],[(3,Invisible)],[(2,Invisible)],[(3,Invisible)],[(4,Invisible)]] D 0
        
        putStr "\n-Checking LookIn Works\n"
        putStr "LookLeft:"
        if lookLeft [[(0,Invisible),(3,Invisible),(2,Invisible),(3,Invisible),(4,Invisible)],[(0,Invisible),(3,Invisible),(2,Invisible),(3,Invisible),(4,Invisible)]] 0 == [[(0,Invisible),(3,Visible),(2,Invisible),(3,Invisible),(4,Visible)],[(0,Invisible),(3,Visible),(2,Invisible),(3,Invisible),(4,Visible)]] then
            putStr "Passed\n"
        else
            do
                putStr "Failed\n"
                print $ lookLeft [[(0,Invisible),(3,Invisible),(2,Invisible),(3,Invisible),(4,Invisible)],[(0,Invisible),(3,Invisible),(2,Invisible),(3,Invisible),(4,Invisible)]] 0
        putStr "LookRight:"
        if lookRight [[(5,Invisible),(3,Invisible),(2,Invisible),(3,Invisible),(0,Invisible)],[(5,Invisible),(3,Invisible),(2,Invisible),(3,Invisible),(0,Invisible)]] 0 == [[(5,Visible),(3,Invisible),(2,Invisible),(3,Visible),(0,Invisible)],[(5,Visible),(3,Invisible),(2,Invisible),(3,Visible),(0,Invisible)]] then
            putStr "Passed\n"
        else
            do
                putStr "Failed\n"
                print $ lookRight [[(5,Invisible),(3,Invisible),(2,Invisible),(3,Invisible),(0,Invisible)],[(5,Invisible),(3,Invisible),(2,Invisible),(3,Invisible),(0,Invisible)]] 0
        putStr "LookTop:"
        if lookTop [[(0,Invisible),(0,Invisible)],[(3,Invisible),(3,Invisible)],[(2,Invisible),(2,Invisible)],[(3,Invisible),(3,Invisible)],[(4,Invisible),(4,Invisible)]] 0 == [[(0,Invisible),(0,Invisible)],[(3,Visible),(3,Visible)],[(2,Invisible),(2,Invisible)],[(3,Invisible),(3,Invisible)],[(4,Visible),(4,Visible)]] then
            putStr "Passed\n"
        else
            do
                putStr "Failed\n"
                print $ lookTop [[(0,Invisible),(0,Invisible)],[(3,Invisible),(3,Invisible)],[(2,Invisible),(2,Invisible)],[(3,Invisible),(3,Invisible)],[(4,Invisible),(4,Invisible)]] 0
        putStr "LookBottom:"
        if lookBottom [[(5,Invisible),(5,Invisible)],[(3,Invisible),(3,Invisible)],[(2,Invisible),(2,Invisible)],[(3,Invisible),(3,Invisible)],[(0,Invisible),(0,Invisible)]] 0 == [[(5,Visible),(5,Visible)],[(3,Invisible),(3,Invisible)],[(2,Invisible),(2,Invisible)],[(3,Visible),(3,Visible)],[(0,Invisible),(0,Invisible)]] then
            putStr "Passed\n"
        else
            do
                putStr "Failed\n"
                print $ lookBottom [[(5,Invisible),(5,Invisible)],[(3,Invisible),(3,Invisible)],[(2,Invisible),(2,Invisible)],[(3,Invisible),(3,Invisible)],[(0,Invisible),(0,Invisible)]] 0
        
        let exampleGrid = translateInput' "30373\n25512\n65332\n33549\n35390"

        putStr "\n-Checking AllDirections Works\n"
        putStr "Test1:"
        if allDirections 2 1 exampleGrid == [1, 2, 1, 2] then
            putStr "Passed\n"
        else
            do
                putStr "Failed\n"
                print $ allDirections 2 1 exampleGrid
        putStr "Test2:"
        if allDirections 2 3 exampleGrid == [2, 2, 2, 1] then
            putStr "Passed\n"
        else
            do
                putStr "Failed\n"
                print $ allDirections 2 3 exampleGrid
        
        putStr "\n-Checking GetLines Works\n"
        putStr "Test1:"
        if getLines 2 1 exampleGrid == [[5,2],[1,2],[3],[3,5,3]] then
            putStr "Passed\n"
        else
            do
                putStr "Failed\n"
                print $ getLines 2 1 exampleGrid
        putStr "Test2:"
        if getLines 2 3 exampleGrid == [[3,3],[4,9],[3,5,3],[3]] then
            putStr "Passed\n"
        else
            do
                putStr "Failed\n"
                print $ getLines 2 3 exampleGrid
        
        putStr "\n-Checking Scan Works\n"
        putStr "Test1:"
        if scan 7 [0,1,0,1,2,7,8] == 6 then
            putStr "Passed\n"
        else
            do
                putStr "Failed\n"
                print $ scan 7 [0,1,0,1,2,7,8]
                
        putStr "\n--- TESTS COMPLETE ---\n\n"

-- Parts
part1 :: [[(Int,Visability)]] -> Int
part1 grid = countVisible $ lookIn $ markEdges $ grid

part2 :: [[Int]] -> Int
part2 grid = maximum [foldr (*) 1 (allDirections x y grid) | x <- [0 .. length (grid !! 0) - 1], y <- [0 .. length grid - 1]]

main = do
        handle <- openFile "d8/d8.txt" ReadMode
        contents <- hGetContents handle
        test
        print $ part2 $ translateInput' contents
        hClose handle
