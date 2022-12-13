import System.IO  
import Control.Monad
import Data.List (transpose)
main = do
        handle <- openFile "d8/d8.txt" ReadMode
        contents <- hGetContents handle
        print $ maximum [product ((\x' y' grid' -> map ((let scan tree xs = if xs == [] then 0 else (if (head (take 1 xs)) >= tree then 1 else 1 + scan tree (drop 1 xs)) in scan) ((grid' !! y') !! x')) $ (\(l, r) -> [reverse l, drop 1 r]) (splitAt x' (grid' !! y')) ++ (\(l, r) -> [reverse l, drop 1 r]) (splitAt y' (transpose grid' !! x'))) x y [[read (x:"")::Int | x <- line] | line <- lines contents]) | x <- [0 .. length (head [[read (x:"")::Int | x <- line] | line <- lines contents]) - 1], y <- [0 .. length [[read (x:"")::Int | x <- line] | line <- lines contents] - 1]]
        hClose handle