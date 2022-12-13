
import System.IO  
import Control.Monad

translateInput :: String -> [(Char, Char)]
translateInput [] = []
translateInput str = [(\ [a, b] -> (a,b)) [x | x <- ls, x `notElem` " "] | ls <- lines str]

getScore :: (Char, Char) -> Int
getScore ('A', 'X') = 4
getScore ('A', 'Y') = 8
getScore ('A', 'Z') = 3
getScore ('B', 'X') = 1
getScore ('B', 'Y') = 5
getScore ('B', 'Z') = 9
getScore ('C', 'X') = 7
getScore ('C', 'Y') = 2
getScore ('C', 'Z') = 6
getScore _ = error "Invalid Input Value [Error 2]"

getTotalScores :: [(Char, Char)] -> Int
getTotalScores = foldr ((+) .  getScore) 0

main = do
        handle <- openFile "d2/d2.txt" ReadMode
        contents <- hGetContents handle
        print $ getTotalScores $ translateInput contents
        hClose handle
