
import System.IO  
import Control.Monad

import Data.List.Split (splitOn)

-- Defining the filesystem
data FileTree = Dir String [FileTree] | File String Int
    deriving (Eq, Show, Read)
data Direction = History String [FileTree]
    deriving (Eq, Show, Read)

type Trail = [Direction]
type Zipper = (FileTree, Trail)

-- Methods to navigate the filesystem
goDir :: Zipper -> String -> Zipper
goDir zipper@(Dir currentName currentContents, trail) dirName | dirExists $ findDir currentContents dirName = ((\(Just dir) -> dir) $ findDir currentContents dirName, History currentName (filter (((\ (Just dir) -> dir) $ findDir currentContents dirName) /=) currentContents):trail)
                                                              | otherwise = zipper
    where
        findDir :: [FileTree] -> String -> Maybe FileTree
        findDir [] _ = Nothing
        findDir ((File _ _):xs) dirName = findDir xs dirName
        findDir ((Dir currentName currentContents):xs) dirName | currentName == dirName = Just (Dir currentName currentContents)
                                                               | otherwise = findDir xs dirName
        dirExists :: Maybe FileTree -> Bool
        dirExists Nothing = False
        dirExists (Just _) = True

goUp :: Zipper -> Zipper
goUp (current, (History name contents):trail) = (Dir name (current:contents), trail)
goUp zipper@(_, []) = zipper

goRoot :: Zipper -> Zipper
goRoot zipper@(_, (History _ _):trail) = goUp zipper
goRoot zipper@(_, []) = zipper

-- Methods to edit the filesystem
makeDir :: Zipper -> String -> Zipper
makeDir zipper@(Dir name contents, trail) dirName | Dir dirName [] `elem` contents = zipper
                                                  | otherwise = (Dir name (Dir dirName []:contents), trail)
makeDir (File _ _, _) _ = error "Should not run makeDir on a File"

makeFile :: Zipper -> String -> Int -> Zipper
makeFile zipper@(Dir name contents, trail) fileName fileSize | File fileName fileSize `elem` contents = zipper
                                                             | otherwise = (Dir name (File fileName fileSize:contents), trail)
makeFile (File _ _, _) _ _ = error "Should not run makeFile on a File"

-- Getting Info
getSum :: [FileTree] -> Int
getSum [] = 0
getSum ((Dir _ cs):xs) = getSum cs + getSum xs
getSum ((File _ x):xs) = x + getSum xs

dirSum :: FileTree -> Int
dirSum (Dir _ xs) = getSum xs
dirSum (File _ _) = error "Should not run dirSum on a File"

zipperSum :: Zipper -> Int
zipperSum zipper = (\(fs,_) -> dirSum fs) (goRoot zipper)

isDir :: FileTree -> Bool
isDir (Dir _ _) = True
isDir (File _ _) = False

-- Creating the filesystem
translateInput :: String -> Zipper
translateInput [] = (Dir "/" [], [])
translateInput str = runCommands (filter ("$ ls" /=) (lines str)) (Dir "/" [], [])

runCommands :: [String] -> Zipper -> Zipper
runCommands [] zipper = goRoot zipper
runCommands ("$ cd ..":xs) zipper = runCommands xs (goUp zipper)
runCommands (('$':' ':'c':'d':' ':x):xs) zipper = runCommands xs (goDir zipper x)
runCommands (('d':'i':'r':' ':x):xs) zipper = runCommands xs (makeDir zipper x)
runCommands (x:xs) zipper = runCommands xs ((\[n, s] -> makeFile zipper s (read n)) $ splitOn " " x)

-- Part 1

dirPickySum :: FileTree -> Int
dirPickySum (Dir _ xs) | getSum xs > 100000 = 0
                       | otherwise = getSum xs
dirPickySum (File _ _) = error "Should not run dirPickySum on a File"

zipperPickySum :: Zipper -> Int
zipperPickySum zipper = (\(fs,_) -> pickySumTotal fs) (goRoot zipper)
    where
        pickySumTotal :: FileTree -> Int
        pickySumTotal dir@(Dir _ xs) = dirPickySum dir + sum (map pickySumTotal xs)
        pickySumTotal (File _ _) = 0

-- Part 2

zipperDeleteDir :: Zipper -> Int
zipperDeleteDir zipper = (\(fs,_) -> minimum $ filter (> getAmountToBeDeleted fs) (getDirSums $ collapseTree fs)) (goRoot zipper)
    where
        collapseTree :: FileTree -> [FileTree]
        collapseTree fs@(Dir _ contents) = concat ([fs]:map collapseTree contents)
        collapseTree fs@(File _ _) = [fs]
        getDirs :: [FileTree] -> [FileTree]
        getDirs fts = filter isDir fts
        getDirSums :: [FileTree] -> [Int]
        getDirSums fts = map dirSum (getDirs fts)
        getAmountToBeDeleted :: FileTree -> Int
        getAmountToBeDeleted fs = 30000000 - (70000000 - dirSum fs)

-- File Handling
main = do
        handle <- openFile "d7/d7.txt" ReadMode
        contents <- hGetContents handle
        print $ zipperDeleteDir $ translateInput contents
        hClose handle
