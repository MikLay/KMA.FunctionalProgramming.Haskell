{-# OPTIONS_GHC -Wall #-}
module Fediuchenko06 where

newtype Poly a = P [a]
-- Задача 1 -----------------------------------------
x :: Num a => Poly a
x = P [0,1]
-- Задача 2 ----------------------------------------
instance (Num a, Eq a) => Eq (Poly a) where
   (P pl1) == (P pl2) = (norZer pl1) == (norZer pl2)

norZer :: (Num a, Eq a) => [a] -> [a]
norZer lst
        | isEmpty lst = []
        | otherwise = (head lst) : norZer (tail lst)
        where
            isEmpty [] = True
            isEmpty (x:y) = x == 0 && isEmpty y
-- Задача 3 -----------------------------------------
ielem :: Num a => [a] -> [(Integer, a)]
ielem [] = []
ielem ls = zip [0 :: Integer ..] ls

instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P pl)
        | null (norZer pl) = "0"
        | otherwise = foldl (++) "" (setBetween ( filter (not . null) shAll))
            where
                shAll = map shOne(reverse (ielem pl))
                shOne(d, c)
                    | d == 0 = show c
                    | d == 1 = (show c) ++ "x"
                    | c == 0 = ""
                    | c == 1 && d == 1 = "x"
                    | c == 1 = "x^" ++ show d
                    | c == -1 = "-x^" ++ show d
                    | c == -1 && d == 1 = "-x"
                    | otherwise = (show c) ++ "x^" ++ (show d)
                setBetween [] = []
                setBetween (w:b)
                    | null b = [w]
                    | otherwise = (w : " + " : (setBetween b))
-- Задача 4 -----------------------------------------
plus :: Num a => Poly a -> Poly a -> Poly a
plus (P []) el = el
plus el (P []) = el
plus (P (ppol1:pol1)) (P (ppol2:pol2)) = P (ppol1+ppol2 : (cf  (P pol1 + P pol2)))

cf :: Num a => Poly a -> [a]
cf (P polinom) = polinom
-- Задача 5 -----------------------------------------
times :: Num a => Poly a -> Poly a -> Poly a
times (P pol1) pol2 = sum [pomn (pol1!!e) e pol2 | e <- [0..(length pol1)-1]]
    where pomn :: (Num a) => a -> Int -> Poly a -> Poly a
          pomn c e (P p) = P (replicate e 0 ++ (map (c*) p))
-- Задача 6 -----------------------------------------
instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate (P p) = P (map ((-1)*) p)
    fromInteger it =  P [(fromInteger it)]
    -- Розумних означень не існує
    abs = undefined
    signum = undefined
-- Задача 7 -----------------------------------------
applyP :: Num a => Poly a -> a -> a
applyP (P pol) x = sum [pol!!e * (x^e) | e <- [0..(length pol)-1]]
-- Задача 8 -----------------------------------------
class Num a => Differentiable a where
    deriv :: a -> a
    nderiv :: Int -> a -> a
    nderiv n x = iterate deriv x !! n
-- Задача 9 -----------------------------------------
instance Num a => Differentiable (Poly a) where
    deriv (P []) = P []
    deriv (P (_:[])) = P []
    deriv (P (_:m)) = x * deriv (P m) + (P m)