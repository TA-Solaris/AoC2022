
import System.IO  
import Control.Monad

import Data.List.Split (splitOn)
import Data.Set (fromList, Set, isSubsetOf)

translateInput :: String -> [[Set Int]]
translateInput [] = []
translateInput str = [[fromList $ (\ (a:b:[]) -> [min a b .. max a b]) [read e | e <- splitOn "-" es] | es <- splitOn "," ls] | ls <- (lines str)]

findSubsets :: [[Set Int]] -> [Bool]
findSubsets [] = []
findSubsets ((a:b:[]):xs) = (isSubsetOf a b || isSubsetOf b a) : findSubsets xs
findSubsets  _ = error "Wrong number of inputs [Error 1]"

countTrue :: [Bool] -> Int
countTrue [] = 0
countTrue (True:xs) = 1 + countTrue xs
countTrue (False:xs) = countTrue xs

main = do  
        let list = []
        handle <- openFile "d4/d4.txt" ReadMode
        contents <- hGetContents handle
        print $ countTrue $ findSubsets $ translateInput contents
        hClose handle
