
import System.IO  
import Control.Monad

import Data.List.Split (splitOn)
import Data.Set (fromList, Set, disjoint)

-- Dumb one line version
getMeAnswer :: String -> Int
getMeAnswer str = length $ filter (\x -> x) (map (\ (a:b:[]) -> not $ disjoint (a::Set Int) (b::Set Int)) [[fromList $ (\ (a:b:[]) -> [min a b .. max a b]) [read e | e <- splitOn "-" es] | es <- splitOn "," ls] | ls <- (lines str)])

main = do  
        let list = []
        handle <- openFile "d4/d4.txt" ReadMode
        contents <- hGetContents handle
        print $ getMeAnswer contents
        hClose handle
