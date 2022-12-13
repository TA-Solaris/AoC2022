import System.IO  
import Control.Monad
import Data.Set (toList, fromList)
a (x:xs) n = if 14 == length (toList $ fromList $ take 14 (x:xs)) then n + 14 else a xs (n+1)
main = do
        handle <- openFile "d6/d6.txt" ReadMode
        contents <- hGetContents handle
        print $ a contents 0
        hClose handle