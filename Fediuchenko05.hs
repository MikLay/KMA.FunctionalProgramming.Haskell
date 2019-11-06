{-# OPTIONS_GHC -Wall #-}
module Fediuchenko05 where
import Control.Monad

type Graph = [[Int]]

-- Задача 1 ------------------------------------------
lucky :: Int ->  [String]
lucky n = filter isLucky [ show x | x <- [(10^(2*n-1))..((10^(2*n))-1)]]

isLucky :: String -> Bool
isLucky st =  sumChars (fst l) == sumChars (snd l)
  where l = splitAt (length st `div` 2) st

sumChars :: String -> Int
sumChars st = foldr (+) 0  (map (read . pure :: Char -> Int) st)


-- Задача 2 -----------------------------------------  
type Board = [Int]
queens ::  Int -> [[Int]]
queens n = cycle [[]] 0
  where 
    cycle :: [Board] -> Int -> [Board]
    cycle boards counter
      | counter == n = boards
      | otherwise = cycle (concatMap expand boards) (counter+1)

    expand :: Board -> [Board]
    expand board = [x : board| x <- [1..n], safe x board 1]

safe :: Int -> Board -> Int -> Bool
safe _ [] _ = True
safe x (a:y) n = and [x /= a, x /= a + n, x /= a - n, safe x y (n+1)]
-- Задача 3 -----------------------------------------
   
maxLen :: [Int] -> Int
maxLen l = length (foldr1 maxByLength  (generateList l))

generateList:: [Int] -> [[Int]]
generateList xs = filter ascending $ foldr (\x xss -> xss ++ map (x:) xss) [[]] xs

ascending :: [Int] -> Bool
ascending [] = False
ascending [_] = True
ascending (x:y:xs) = and [x < y, ascending (y:xs)]

maxByLength :: [Int] -> [Int] -> [Int]
maxByLength xs xy = if length xs >= length xy then xs else xy


-- Задача 4 -----------------------------------------
maxSeq ::  [Int] ->  [Int]
maxSeq  l = foldr1 maxByLength (generateList l)
  

-- Задача 5 -----------------------------------------
allMaxSeq ::  [Int] -> [[Int]]
allMaxSeq l =  filter (\x -> (length x) == size) list
    where
    list = generateList l
    size = length (foldr1 maxByLength list)

-- Задача 6 -----------------------------------------

permutationsRep xs = iterate (liftM2 (:) xs) [[]]

evalPol :: [String] -> Int
evalPol = head. (foldl stEv [])
 where stEv (x:y:xs) "+" = (y+x):xs
       stEv (x:y:xs) "-" = (x-y):xs
       stEv (x:y:xs) "*" = (y*x):xs
       stEv xs st = (read st):xs

toDigs :: Int -> [String]
toDigs 0 = []
toDigs x = toDigs (x `div` 10) ++ [show (x `mod` 10)]


genExpr ::  Int -> Int -> [String]
genExpr n s = map f (filter mfilter (permutationsRep ["+", "-", "*"] !! ((length nd)-1)))
  where 
  nd = toDigs n 
  f x = foldr1 (++) (merge nd x)
  mfilter ops = s == evalPol ((reverse nd) ++ ops)

merge :: [String] -> [String] -> [String]
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) = x : y : merge xs ys

-- Задача 7 -----------------------------------------
genExprBracket ::  Int -> Int -> [String]
genExprBracket = undefined

-- Задача 8 -----------------------------------------
topolSortAll :: Graph -> [[Int]]
topolSortAll = undefined

--------------------------------------------
gr1 :: Graph 
gr1 = [[1,2,3], [], [3,4], [4],[]]