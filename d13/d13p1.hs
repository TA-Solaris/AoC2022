
import System.IO  
import Control.Monad

import Prelude hiding (compare)

import Data.List.Split (splitOn)

-- Functional parsing library from chapter 13 of Programming in Haskell,
-- Graham Hutton, Cambridge University Press, 2016.
import Parsing

-- Defining Data Types
data Packet = Item Int | List [Packet] deriving (Eq, Show)

-- Translating Input
translateInput :: String -> [(Packet, Packet)]
translateInput [] = []
translateInput str = map ((\[a, b] -> (a, b)) . map (extractParsed . parse parseExpr) . splitOn "\n") (splitOn "\n\n" str)

-- Parsing

extractParsed :: [(a, String)] -> a
extractParsed [(a, "")] = a
extractParsed _ = error "Passing failed or multiple parses generated [Error 1]"

-- BNF
-- Expr ::= Item | "[]" | '[' ExprList ']'
-- ExprList ::= Expr | Expr ',' ExprList

parseExpr :: Parser Packet
parseExpr = parseItem <|> parseEmpty <|> do symbol "["
                                            es <- parseExprList
                                            symbol "]"
                                            return es

parseItem :: Parser Packet
parseItem = Item <$> int

parseEmpty :: Parser Packet
parseEmpty = do symbol "[]"
                return $ List []

parseExprList :: Parser Packet
parseExprList = do e <- parseExpr
                   es <- many (do symbol ","
                                  parseExpr)
                   return $ List (e:es)

-- Comparing
compare :: Packet -> Packet -> Ordering
compare (Item a) (Item b) | a < b = LT
                          | a > b = GT
                          | a == b = EQ
compare (List (a:as)) (List (b:bs)) | compare a b == EQ = compare (List as) (List bs)
                                    | otherwise = compare a b
compare (List []) (List []) = EQ
compare (List []) (List _) = LT
compare (List _) (List []) = GT
compare (Item a) (List bs) = compare (List [Item a]) (List bs)
compare (List as) (Item b) = compare (List as) (List [Item b])

-- Parts
part1 :: [(Packet, Packet)] -> Int
part1 xs = sum [i | (c, i) <- zip (map (uncurry compare) xs) [1 .. length xs], c /= GT]

main = do
        handle <- openFile "d13/d13.txt" ReadMode
        contents <- hGetContents handle
        print $ part1 $ translateInput contents
        hClose handle
