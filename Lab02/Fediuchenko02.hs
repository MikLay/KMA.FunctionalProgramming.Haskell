{-# OPTIONS_GHC -Wall #-}
module HWI02 where

-- ?????? 1 -----------------------------------------
sumFr :: [Integer] -> Integer
sumFr xs = foldr (+) 0 xs
  
-- ?????? 2 ----------------------------------------- 
factorial :: Integer -> Integer
factorial n = foldl (*) 1 [2..n]

-- ?????? 3 -----------------------------------------
concatFr :: [Integer] -> [Integer] -> [Integer]
concatFr xs ys = foldr (:) xs ys

-- ?????? 4 -----------------------------------------
insert :: [Integer] -> Integer -> [Integer]
insert as a = [x | x <- as, x < a] ++ [a] ++ [x | x <- as, x >= a]

sortInsert:: [Integer] -> [Integer]
sortInsert xs = foldl (insert) [] xs
 
-- ?????? 5 -----------------------------------------
map2 :: (a->b->c) -> [a] -> [b] -> [c]
map2 f (x:xs) (y:ys) = (f x y):map2 f xs ys
map2 _ _ _ = []

-- ?????? 6 -----------------------------------------
expPart :: Integer -> Integer -> Double
expPart m n = sum [(fromIntegral(m)^i)/fromIntegral(factorial i) | i <- [1..n]]

-- ?????? 7 -----------------------------------------
triangle :: [Integer]
triangle = scanl (\x y -> x + y) 1 [2..]

-- ?????? 8 -----------------------------------------
piramid :: [Integer]
piramid = scanl (\x y -> x + y*y) 1 [2..]

-- ?????? 9 -----------------------------------------
indexes :: [Int] -> [Int] -> [Int]
indexes xs ys = [ i | i <- [0..length ys], (take (length xs) (drop i ys)) == xs ]
