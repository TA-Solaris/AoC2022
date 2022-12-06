
import System.IO  
import Control.Monad

import Data.Set (toList, fromList)

-- Chaotic evil coding
doTaTing (x:xs) n = case (14 == (length $ toList $ fromList $ take 14 (x:xs))) of {True -> n; False -> doTaTing xs (n+1)}

main = do
        handle <- openFile "d6/d6.txt" ReadMode
        contents <- hGetContents handle
        print $ doTaTing contents 14
        hClose handle
