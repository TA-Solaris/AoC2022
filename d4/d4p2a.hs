import System.IO  
import Control.Monad
import Data.List.Split (splitOn)
import Data.Set (fromList, Set, disjoint)
main = do
        handle <- openFile "d4/d4.txt" ReadMode
        contents <- hGetContents handle
        print $ length $ filter id (map (\[a, b] -> not $ disjoint (a::Set Int) (b::Set Int)) [[fromList $ (\[a, b] -> [min a b .. max a b]) [read e | e <- splitOn "-" es] | es <- splitOn "," ls] | ls <- lines contents])
        hClose handle