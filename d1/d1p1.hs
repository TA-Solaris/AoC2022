
import System.IO  
import Control.Monad

import Data.List.Split (splitOn)

translateInput :: String -> [[Int]]
translateInput [] = []
translateInput str = [[read x | x <- (lines el)] | el <- (splitOn "\n\n" str)]

getElfSums :: [[Int]] -> [Int]
getElfSums [] = []
getElfSums xs = [sum x | x <- xs]

getMaxCalories :: [Int] -> Int
getMaxCalories [] = error "No Elfs in Input [Error 1]"
getMaxCalories xs = maximum xs

main = do  
        let list = []
        handle <- openFile "d1/d1.txt" ReadMode
        contents <- hGetContents handle
        print $ getMaxCalories $ getElfSums $ translateInput contents
        hClose handle
