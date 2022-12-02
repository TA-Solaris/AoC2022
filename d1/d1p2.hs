
import System.IO  
import Control.Monad

import Data.List.Split (splitOn)
import Data.List (sort)

translateInput :: String -> [[Int]]
translateInput [] = []
translateInput str = [[read x | x <- (lines el)] | el <- (splitOn "\n\n" str)]

getElfSums :: [[Int]] -> [Int]
getElfSums [] = []
getElfSums xs = [sum x | x <- xs]

getMaxCalories :: [Int] -> Int
getMaxCalories [] = error "No Elfs in Input [Error 1]"
getMaxCalories xs = maximum xs

getTopThree :: [Int] -> Int
getTopThree xs = child $ reverse $ sort xs
    where
        child :: [Int] -> Int
        child (a:b:c:_) = sum [a,b,c]
        child _ = error "Not enough elves [Error 2]"

main = do  
        let list = []
        handle <- openFile "d1/d1.txt" ReadMode
        contents <- hGetContents handle
        print $ getTopThree $ getElfSums $ translateInput contents
        hClose handle
