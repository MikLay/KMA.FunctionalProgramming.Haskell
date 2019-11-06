{-# OPTIONS_GHC -Wall #-}
module HWI07 where

data Stream a = Cons a (Stream a)

-- Екземпляр Show виводить перші 20 елементів, за якими розташовані крапки продовження
instance Show a => Show (Stream a) where
    show xs =  (foldl (++) "[" 
                  $ map (\x -> (show x) ++ ", ") $ take 20 $ streamToList xs
                ) ++ "..."

--Задача 1 -----------------------------------------
streamToList :: Stream a -> [a]
streamToList (Cons i ls) = i : streamToList ls

-- Задача 2 -----------------------------------------
instance Functor Stream where
    fmap func (Cons i ls) = Cons (func i)  (fmap func ls)


-- Задача 3 -----------------------------------------
sRepeat :: a -> Stream a
sRepeat x = Cons x $ sRepeat x

sIterate :: (a -> a) -> a -> Stream a
sIterate orule x = Cons x $ sIterate  orule $ orule x

sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (Cons s ls) ls1  = Cons s (sInterleave ls1 ls)

sTake :: Int -> Stream a -> [a]
sTake 0 (Cons _ _) = []
sTake n (Cons x ls) = take n $ streamToList $ Cons x ls

-- Задача 4 -----------------------------------------
nats :: Stream Integer
nats = sIterate (+1) 0

-- Задача 5 -----------------------------------------
ruler :: Stream Integer
ruler = fmap fu (sIterate (+1) 1)
  where fu x | odd x = 0
             | otherwise = fu (x `div` 2) + 1

-- Задача 6 -----------------------------------------
rand :: Integer -> Stream Integer
rand n = if n > 0
            then 
               sIterate (\x -> (1103515245 * x + 12345) `mod` 2147483648) n
            else
               error "Must be n >= 1"

-- Задача 7 -----------------------------------------
fib :: Integer -> Integer
fib n
  | n == 0 = 1
  | n == 1 = 1
  | otherwise = fib(n - 1) + fib(n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Задача 8 -----------------------------------------
fibs2 :: [Integer]
fibs2 = 1 : 1 : zipWith (+) fibs2 (tail fibs2)

-- Задача 9 -----------------------------------------
data  Matrix a = M(a,a)(a,a)
         deriving (Show, Eq, Ord)
         
instance Num a => Num (Matrix a) where
    (+) (M(a1, b1)(c1,d1)) (M(a2, b2)(c2, d2)) = M(a1+a2, b1+b2)(c1+c2, d1+d2)
    (*) (M(a1,b1)(c1,d1)) (M(a2,b2)(c2,d2)) = M(a1*a2 + b1*c2, a1*b2+b1*d2)(c1*a2+d1*c2, c1*b2 + d1*d2)
    negate (M(x1,y1)(x2,y2)) = M(-x1,-y1)(-x2,-y2)
    fromInteger i = M(fromInteger i,0)(0,0) 
    -- Реалізовувати не потрібно
    abs    = undefined
    signum = undefined

-- Задача 10 ----------------------------------------
fastFib :: Integer -> Integer
fastFib n = getLeftT (M(1,1)(1,0) ^ n)

getLeftT :: Matrix a -> a
getLeftT (M(x,_) _) = x