
import System.IO  
import Control.Monad

import Prelude hiding (round)

import Data.List.Split (splitOn)
import Data.Text (replace, pack, unpack)

import Data.List (sort)

-- Defining Types
data Monkey = Monkey [Int] (Int -> Int) (Int -> Int) Int
--    deriving (Show, Read, Eq)

-- Translating Input
translateInput :: String -> ([Monkey], Int -> Int)
translateInput [] = ([],(*1))
translateInput str = (cleanInput str, calm $ product $ cleanInput' str)
    where
        cleanInput :: String -> [Monkey]
        cleanInput str = [Monkey [read x | x <- splitOn "," (splitOn ":" monkey !! 1)] ((\sec -> if sec == "old" then operation '^' 0 else operation (head $ head $ splitOn " " (splitOn ":" monkey !! 2)) (read (splitOn " " (splitOn ":" monkey !! 2) !! 1))) (splitOn " " (splitOn ":" monkey !! 2) !! 1)) (test (read (splitOn ":" monkey !! 3)) (read (splitOn ":" monkey !! 4)) (read (splitOn ":" monkey !! 5))) 0 | monkey <- splitOn "\n\n" $ unpack $ replace (pack "\n    If false: throw to monkey ") (pack ":") $ replace (pack "\n    If true: throw to monkey ") (pack ":") $ replace (pack "\n  Test: divisible by ") (pack ":") $ replace (pack "\n  Operation: new = old ") (pack ":") $ replace (pack "\n  Starting items: ") (pack "") $ replace (pack ", ") (pack ",") $ replace (pack "Monkey ") (pack "") (pack str)]
        cleanInput' :: String -> [Int]
        cleanInput' str = [read (splitOn ":" monkey !! 3) | monkey <- splitOn "\n\n" $ unpack $ replace (pack "\n    If false: throw to monkey ") (pack ":") $ replace (pack "\n    If true: throw to monkey ") (pack ":") $ replace (pack "\n  Test: divisible by ") (pack ":") $ replace (pack "\n  Operation: new = old ") (pack ":") $ replace (pack "\n  Starting items: ") (pack "") $ replace (pack ", ") (pack ",") $ replace (pack "Monkey ") (pack "") (pack str)]

-- Defining helper functions
operation :: Char -> Int -> Int -> Int
operation '*' a b = b * a
operation '+' a b = b + a
operation '^' _ b = b * b
operation _ _ _ = error "Invalid operation [Error 1]"

calm :: Int -> Int -> Int
calm cv x = x `mod` cv

test :: Int -> Int -> Int -> Int -> Int
test tv tm fm v | (v `mod` tv) == 0 = tm
                | otherwise = fm

nreplace :: Int -> a -> [a] -> [a]
nreplace _ _ [] = []
nreplace n v (x:xs) | n == 0 = v:xs
                    | otherwise = x:nreplace (n-1) v xs

throw :: Int -> Int -> [Monkey] -> [Monkey]
throw from to monkeys = nreplace from ((\(Monkey (_:list) op tst count) -> Monkey list op tst count) (monkeys !! from)) (nreplace to ((\(Monkey list op tst count) -> Monkey (list ++ [head $ getItems (monkeys !! from)]) op tst count) (monkeys !! to)) monkeys)

getItems :: Monkey -> [Int]
getItems (Monkey items _ _ _) = items

getOperation :: Monkey -> (Int -> Int)
getOperation (Monkey _ op _ _) = op

getTest :: Monkey -> (Int -> Int)
getTest (Monkey _ _ tst _) = tst

getCount :: Monkey -> Int
getCount (Monkey _ _ _ count) = count

turn :: Int -> [Monkey] -> (Int -> Int) -> [Monkey]
turn i monkeys calm' | null (getItems (monkeys !! i)) = monkeys
                     | otherwise = turn i (throw i (getTest (monkeys !! i) (calm' $ getOperation (monkeys !! i) $ head $ getItems (monkeys !! i))) (nreplace i (Monkey (calm' (getOperation (monkeys !! i) ((\(x:_) -> x) $ getItems (monkeys !! i))):(\(_:xs) -> xs) (getItems (monkeys !! i))) (getOperation (monkeys !! i)) (getTest (monkeys !! i)) (getCount (monkeys !! i) + 1)) monkeys)) calm'

round :: [Monkey] -> (Int -> Int) -> [Monkey]
round monkeys calm' = roundHelper 0 monkeys calm'
    where
        roundHelper :: Int -> [Monkey] -> (Int -> Int) -> [Monkey]
        roundHelper i monkeys calm' | i == length monkeys = monkeys
                                    | otherwise = roundHelper (i + 1) (turn i monkeys calm') calm'

rounds :: Int -> [Monkey] -> (Int -> Int) -> [Monkey]
rounds n monkeys calm' | n == 0 = monkeys
                       | otherwise = rounds (n - 1) (round monkeys calm') calm'

getMonkeyBuisness :: [Monkey] -> Int
getMonkeyBuisness monkeys = (\(a:b:_) -> a * b) $ reverse $ sort [getCount monkey | monkey <- monkeys]

getMonkeyItems :: [Monkey] -> [[Int]]
getMonkeyItems monkeys = [getItems monkey | monkey <- monkeys]

getMonkeyCounts :: [Monkey] -> [Int]
getMonkeyCounts monkeys = [getCount monkey | monkey <- monkeys]

main = do
        handle <- openFile "d11/d11.txt" ReadMode
        contents <- hGetContents handle
        print $ getMonkeyBuisness $ uncurry (rounds 10000) (translateInput contents)
        hClose handle
