
import System.IO  
import Control.Monad

import Data.List.Split (splitOn)

translateInput :: String -> String
translateInput [] = []
translateInput str = str

main = do
        handle <- openFile "d?/d?.txt" ReadMode
        contents <- hGetContents handle
        print $ translateInput contents
        hClose handle
