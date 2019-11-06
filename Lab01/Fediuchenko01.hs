{-# OPTIONS_GHC -Wall #-}
module HWI01 where

-- Задача 1 -----------------------------------------
factorial :: Integer -> Integer
factorial x = if x == 0 then 1 else x * factorial (x-1) 

-- Задача 2 -----------------------------------------
listSum :: [Int] -> [Int] -> [Int]
listSum xs ys = if null xs then ys else if null ys then xs else (head xs + head ys):listSum (tail xs) (tail ys)

-- Задача 3 ----------------------------------------- 
oddEven :: [Int] -> [Int] 
oddEven [xs] = [xs]
oddEven [] = []
oddEven (x1:x2:xs) = x2:x1:oddEven xs 

-- Задача 4 -----------------------------------------
position    ::  Int -> [Int] -> Int
position n xs | xs == [] = -1
              | head xs == n = 0
              | otherwise = 1 + (position  n (tail xs))
                   
-- Задача 5 -----------------------------------------
set :: [Int] -> [Int] 
set [] = []
set [xs] = [xs]
set (x:xs) = if notElem x xs then x:set xs else set xs
-- Задача 6 -----------------------------------------
union :: [Int] -> [Int] -> [Int]
union xs ys = set (xs++ys)

-- Задача 7 -----------------------------------------
intersection :: [Int] -> [Int] -> [Int]
intersection xs ys = set ([x | y <- ys, x <-xs, x==y])

-- Задача 8 -----------------------------------------
factorialsM :: [Integer]
factorialsM = [factorial x | x <- [1..]] 