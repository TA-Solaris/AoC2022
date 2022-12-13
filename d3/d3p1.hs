
import System.IO  
import Control.Monad

import Data.List
import Data.Char (ord)

translateInput :: String -> [Char]
translateInput [] = []
translateInput str = [helper $ splitAt (length ls `div` 2) ls | ls <- lines str]
    where
        helper :: (String, String) -> Char
        helper (a,b) = head (a `intersect` b)

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
