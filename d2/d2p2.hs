
import System.IO  
import Control.Monad

translateInput :: String -> [(Char, Char)]
translateInput [] = []
translateInput str = [(\ (a:b:[]) -> (a,b)) [x | x <- ls, not (x `elem` " ")] | ls <- (lines str)]

getScore :: (Char, Char) -> Int
getScore ('A', 'X') = 3
getScore ('A', 'Y') = 4
getScore ('A', 'Z') = 8
getScore ('B', 'X') = 1
getScore ('B', 'Y') = 5
getScore ('B', 'Z') = 9
getScore ('C', 'X') = 2
getScore ('C', 'Y') = 6
getScore ('C', 'Z') = 7
getScore _ = error "Invalid Input Value [Error 2]"

getTotalScores :: [(Char, Char)] -> Int
getTotalScores (x:xs) = getScore x + getTotalScores xs
getTotalScores [] = 0

main = do  
        let list = []
        handle <- openFile "d2/d2.txt" ReadMode
        contents <- hGetContents handle
        print $ getTotalScores $ translateInput contents
        hClose handle
