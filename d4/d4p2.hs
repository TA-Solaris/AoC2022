
import System.IO  
import Control.Monad

import Data.List.Split (splitOn)
import Data.Set (fromList, Set, disjoint)

translateInput :: String -> [[Set Int]]
translateInput [] = []
translateInput str = [[fromList $ (\ (a:b:[]) -> [min a b .. max a b]) [read e | e <- splitOn "-" es] | es <- splitOn "," ls] | ls <- (lines str)]

findIntersects :: [[Set Int]] -> [Bool]
findIntersects [] = []
findIntersects ((a:b:[]):xs) = not (disjoint a b) : findIntersects xs
findIntersects  _ = error "Wrong number of inputs [Error 1]"

countTrue :: [Bool] -> Int
countTrue [] = 0
countTrue (True:xs) = 1 + countTrue xs
countTrue (False:xs) = countTrue xs

main = do  
        let list = []
        handle <- openFile "d4/d4.txt" ReadMode
        contents <- hGetContents handle
        print $ countTrue $ findIntersects $ translateInput contents
        hClose handle
