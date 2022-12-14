
import System.IO  
import Control.Monad

import Data.Set (toList, fromList)

-- Chaotic evil coding
doTaTing :: String -> Int
doTaTing str = evilChild str 14
    where
        -- Muahahaha infinite power
        evilChild :: String -> Int -> Int
        -- Welcome to hell
        evilChild (x:xs) n | 14 == length (toList $ fromList $ take 14 (x:xs)) = n
                           | otherwise = evilChild xs (n+1)

main = do
        handle <- openFile "d6/d6.txt" ReadMode
        contents <- hGetContents handle
        print $ doTaTing contents
        hClose handle
