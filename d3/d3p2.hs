
import System.IO  
import Control.Monad

import Data.List
import Data.Char (ord)

translateInput :: String -> [Char]
translateInput [] = []
translateInput str = helper $ lines str
    where
        helper :: [String] -> [Char]
        helper [] = []
        helper (a:b:c:xs) = head ((a `intersect` b) `intersect` c) : helper xs
        helper _ = error "Invalid Input Number of Lines [Error 2]"

getValue :: Char -> Int
getValue x | x `elem` ['A' .. 'Z'] = ord x - ord 'A' + 27
           | x `elem` ['a' .. 'z'] = ord x - ord 'a' + 1
           | otherwise = error "Invalid Character in Input [Error 1]"

getSum :: [Char] -> Int
getSum = foldr ((+) . getValue) 0

main = do
        handle <- openFile "d3/d3.txt" ReadMode
        contents <- hGetContents handle
        print $ getSum $ translateInput contents
        hClose handle
