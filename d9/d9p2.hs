
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
translateInput str = concat [(\[c, n] -> replicate (read n) (read c)) $ splitOn " " line | line <- lines str]

-- Defining helper functions
absDiff :: Num a => a -> a -> a
absDiff n = abs . (n-)

move :: End -> Command -> End
move (Head (hx, hy)) U = Head (hx, hy - 1)
move (Head (hx, hy)) D = Head (hx, hy + 1)
move (Head (hx, hy)) L = Head (hx - 1, hy)
move (Head (hx, hy)) R = Head (hx + 1, hy)
move _ _ = error "Should only move heads [Error 3]"

follow :: (End, End) -> (End, End)
follow es@(Head (hx, hy), Tail (tx, ty) his) | absDiff hx tx > 1 && hx > tx && hy == ty = (Head (hx, hy), Tail (tx + 1, ty) (insert (tx + 1, ty) his)) -- Right
                                             | absDiff hx tx > 1 && hx < tx && hy == ty = (Head (hx, hy), Tail (tx - 1, ty) (insert (tx - 1, ty) his)) -- Left
                                             | absDiff hy ty > 1 && hy > ty && hx == tx = (Head (hx, hy), Tail (tx, ty + 1) (insert (tx, ty + 1) his)) -- Down
                                             | absDiff hy ty > 1 && hy < ty && hx == tx = (Head (hx, hy), Tail (tx, ty - 1) (insert (tx, ty - 1) his)) -- Up
                                             | (absDiff hx tx > 1 || absDiff hy ty > 1) && hx > tx && hy > ty = (Head (hx, hy), Tail (tx + 1, ty + 1) (insert (tx + 1, ty + 1) his)) -- Down Right
                                             | (absDiff hx tx > 1 || absDiff hy ty > 1) && hx > tx && hy < ty = (Head (hx, hy), Tail (tx + 1, ty - 1) (insert (tx + 1, ty - 1) his)) -- Up Right
                                             | (absDiff hx tx > 1 || absDiff hy ty > 1) && hx < tx && hy > ty = (Head (hx, hy), Tail (tx - 1, ty + 1) (insert (tx - 1, ty + 1) his)) -- Down Left
                                             | (absDiff hx tx > 1 || absDiff hy ty > 1) && hx < tx && hy < ty = (Head (hx, hy), Tail (tx - 1, ty - 1) (insert (tx - 1, ty - 1) his)) -- Up Left
                                             | otherwise = es
follow es@(Head (hx, hy), Head (tx, ty)) | absDiff hx tx > 1 && hx > tx && hy == ty = (Head (hx, hy), Head (tx + 1, ty)) -- Right
                                         | absDiff hx tx > 1 && hx < tx && hy == ty = (Head (hx, hy), Head (tx - 1, ty)) -- Left
                                         | absDiff hy ty > 1 && hy > ty && hx == tx = (Head (hx, hy), Head (tx, ty + 1)) -- Down
                                         | absDiff hy ty > 1 && hy < ty && hx == tx = (Head (hx, hy), Head (tx, ty - 1)) -- Up
                                         | (absDiff hx tx > 1 || absDiff hy ty > 1) && hx > tx && hy > ty = (Head (hx, hy), Head (tx + 1, ty + 1)) -- Down Right
                                         | (absDiff hx tx > 1 || absDiff hy ty > 1) && hx > tx && hy < ty = (Head (hx, hy), Head (tx + 1, ty - 1)) -- Up Right
                                         | (absDiff hx tx > 1 || absDiff hy ty > 1) && hx < tx && hy > ty = (Head (hx, hy), Head (tx - 1, ty + 1)) -- Down Left
                                         | (absDiff hx tx > 1 || absDiff hy ty > 1) && hx < tx && hy < ty = (Head (hx, hy), Head (tx - 1, ty - 1)) -- Up Left
                                         | otherwise = es
follow _ = error "Must be in the format (Head, Tail) or (Head, Head) [Error 1]"

followAll :: [End] -> [End]
followAll (a:b:xs) = (\(f, s) -> f : followAll (s:xs)) $ follow (a, b)
followAll x = x

simulate :: [End] -> [Command] -> [End]
simulate state [] = state
simulate (head:state) (cmd:cmds) = simulate (followAll (move head cmd:state)) cmds

historySize :: [End] -> Int
historySize [] = error "Must have a tail [Error 2]"
historySize ((Head _):es) = historySize es
historySize ((Tail _ his):_) = length his

-- Parts
part2 :: [Command] -> Int
part2 cmds = historySize $ simulate (replicate 9 (Head (0, 0)) ++ [Tail (0, 0) (singleton (0, 0))]) cmds

main = do
        handle <- openFile "d9/d9.txt" ReadMode
        contents <- hGetContents handle
        print $ part2 $ translateInput contents
        hClose handle
