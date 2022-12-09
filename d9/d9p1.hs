
import System.IO  
import Control.Monad

import Data.List.Split (splitOn)
import Data.Set (Set, singleton, insert)

-- Defining Data Types
type Position = (Int, Int)

type History = (Set Position)

data End = Head Position | Tail Position History
    deriving (Show, Read, Eq)

data Command = U | D | L | R
    deriving (Show, Read, Eq)

-- For translating the input
translateInput :: String -> [Command]
translateInput [] = []
translateInput str = foldr (++) [] [(\(c:n:[]) -> replicate (read n) (read c)) $ splitOn " " line | line <- lines str]

-- Defining helper functions
absDiff :: Num a => a -> a -> a
absDiff n = abs . (n-)

notSameCol :: Position -> Position -> Bool
notSameCol (hx, hy) (tx, ty) = (absDiff hx tx >= 2 && absDiff hy ty >= 1) || (absDiff hx tx >= 1 && absDiff hy ty >= 2)

sameCol :: Position -> Position -> Bool
sameCol (hx, hy) (tx, ty) = (absDiff hx tx >= 2 && absDiff hy ty == 0) || (absDiff hx tx == 0 && absDiff hy ty >= 2)

sim1 :: (End, End) -> Command -> (End, End)
sim1 (Head (hx, hy), Tail (tx, ty) his) U | notSameCol (hx, hy - 1) (tx, ty) && hx < tx = (Head (hx, hy - 1), Tail (tx - 1, ty - 1) (insert (tx - 1, ty - 1) his))
                                          | notSameCol (hx, hy - 1) (tx, ty) && hx > tx = (Head (hx, hy - 1), Tail (tx + 1, ty - 1) (insert (tx + 1, ty - 1) his))
                                          | sameCol (hx, hy - 1) (tx, ty) = (Head (hx, hy - 1), Tail (tx, ty - 1) (insert (tx, ty - 1) his))
                                          | otherwise = (Head (hx, hy - 1), Tail (tx, ty) his)
sim1 (Head (hx, hy), Tail (tx, ty) his) D | notSameCol (hx, hy + 1) (tx, ty) && hx < tx = (Head (hx, hy + 1), Tail (tx - 1, ty + 1) (insert (tx - 1, ty + 1) his))
                                          | notSameCol (hx, hy + 1) (tx, ty) && hx > tx = (Head (hx, hy + 1), Tail (tx + 1, ty + 1) (insert (tx + 1, ty + 1) his))
                                          | sameCol (hx, hy + 1) (tx, ty) = (Head (hx, hy + 1), Tail (tx, ty + 1) (insert (tx, ty + 1) his))
                                          | otherwise = (Head (hx, hy + 1), Tail (tx, ty) his)
sim1 (Head (hx, hy), Tail (tx, ty) his) L | notSameCol (hx - 1, hy) (tx, ty) && hy < ty = (Head (hx - 1, hy), Tail (tx - 1, ty - 1) (insert (tx - 1, ty - 1) his))
                                          | notSameCol (hx - 1, hy) (tx, ty) && hy > ty = (Head (hx - 1, hy), Tail (tx - 1, ty + 1) (insert (tx - 1, ty + 1) his))
                                          | sameCol (hx - 1, hy) (tx, ty) = (Head (hx - 1, hy), Tail (tx - 1, ty) (insert (tx - 1, ty) his))
                                          | otherwise = (Head (hx - 1, hy), Tail (tx, ty) his)
sim1 (Head (hx, hy), Tail (tx, ty) his) R | notSameCol (hx + 1, hy) (tx, ty) && hy < ty = (Head (hx + 1, hy), Tail (tx + 1, ty - 1) (insert (tx + 1, ty - 1) his))
                                          | notSameCol (hx + 1, hy) (tx, ty) && hy > ty = (Head (hx + 1, hy), Tail (tx + 1, ty + 1) (insert (tx + 1, ty + 1) his))
                                          | sameCol (hx + 1, hy) (tx, ty) = (Head (hx + 1, hy), Tail (tx + 1, ty) (insert (tx + 1, ty) his))
                                          | otherwise = (Head (hx + 1, hy), Tail (tx, ty) his)
sim1 _ _ = error "Must be in the format (Head, Tail) [Error 1]"

simulate :: (End, End) -> [Command] -> (End, End)
simulate state [] = state
simulate state (cmd:cmds) = simulate (sim1 state cmd) cmds

historySize :: (End, End) -> Int
historySize (Head _, Tail _ his) = length his
historySize _ = error "Must be in the format (Head, Tail) [Error 1]"

-- Parts
part1 :: [Command] -> Int
part1 cmds = historySize $ simulate (Head (0, 0), Tail (0, 0) (singleton (0, 0))) cmds

main = do
        handle <- openFile "d9/d9.txt" ReadMode
        contents <- hGetContents handle
        print $ part1 $ translateInput contents
        hClose handle
