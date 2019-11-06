{-# OPTIONS_GHC -Wall #-}
module Fediuchenko_Lab03 where

type Code = String
data Move = Move Code Int Int
          deriving (Show, Eq)

-- Завдання 1 -----------------------------------------
exactMatches :: Code -> Code -> Int
exactMatches [] _ = 0
exactMatches _ [] = 0
exactMatches (x:xs) (y:ys)
    | x == y = 1 + exactMatches xs ys
    | otherwise = exactMatches xs ys

-- Завдання 2 -----------------------------------------
countDigits :: Code -> [Int]
countDigits cs = [count x cs| x <- ['0'..'9']] where
    count _ [] = 0
    count y (x:xs)
        | y == x = 1 + count y xs
        | otherwise = count y xs

-- Завдання 3 ----------------------------------------- 
matches :: Code -> Code -> Int
matches cs as = sum (takeMin (countDigits cs) (countDigits as)) where    
    takeMin _ [] = []
    takeMin [] _ = []
    takeMin (x:xs) (y:ys) = (min x y) : (takeMin xs ys)
 
-- Завдання 4 -----------------------------------------
getMove :: Code -> Code -> Move
getMove cs as =
    let bulls = exactMatches cs as
        cows = (matches cs as) - bulls
    in Move as bulls cows

-- Завдання 5 -----------------------------------------   
isConsistent :: Move -> Code -> Bool
isConsistent (Move as bulls cows) cs = (Move as bulls cows) == (getMove cs as)

-- Завдання 6 -----------------------------------------
filterCodes :: Move -> [Code] -> [Code]
filterCodes mv cdx = filter (isConsistent mv) cdx

-- Завдання 7 -----------------------------------------
allCodes :: Int -> [Code]
ns :: [Char]
ns= ['0'..'9']
allCodes 1 = [[num] | num <- ns]
allCodes n = concatMap (\cdx -> [cdx++[num] | num <- ns]) (allCodes (n-1))
   
-- Завдання 8 -----------------------------------------
solve :: Code -> [Move]
solve code = 
    let codes = allCodes (length code)    
    in generateSolution code codes where
    generateSolution _ [] = []
    generateSolution cd [att] = [getMove cd att]
    generateSolution cd (att:cdx) = 
        let mv = getMove cd att
        in mv : (generateSolution cd (filterCodes mv cdx))