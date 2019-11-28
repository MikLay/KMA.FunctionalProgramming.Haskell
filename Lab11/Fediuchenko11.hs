{-# OPTIONS_GHC -Wall #-}
module Fediuchenko11 where

import Data.Maybe
import Data.List
import Data.Char(isLower)

data BExp = Bvalue Bool | Bvar Char | Not BExp | And BExp BExp | Or BExp BExp
            deriving (Eq, Ord, Show)
type Env = [(Char, Bool)]

type NodeId = Int
type BDDNode =  (NodeId, (Char, NodeId, NodeId))
type BDD = (NodeId, [BDDNode])

type Index = Char

-- Задача 1 -----------------------------------------
checkSat :: BDD -> Env -> Bool
checkSat (0, nodes) env = False
checkSat (1, nodes) env = True
checkSat (id, nodes) env
  | gRi  = checkSat (rightValue, nodes) env
  | otherwise = checkSat (leftValue, nodes) env
  where
    (index, leftValue, rightValue) = getN id nodes
    gRi = getN index env

getN :: Eq a => a -> [(a, b)] -> b
getN x ys = fromJust(lookup x ys)  


-- Задача 2 -----------------------------------------

sat :: BDD -> [[(Char, Bool)]]
sat bdd@(_, nodes) = if (null nodes) then [] else sort ( filter (checkSat bdd) (generateFunct (getValuesFunct nodes)))

processFunct env xs n = if nxt > 1 then (processFunct env xs) nxt else nxt
  where nxt = (getNextOneFunct env xs) n

getNextOneFunct env xs i = if (getValueFunct1 env) idx then t else f
  where (_, (idx, f, t)) = doGet i xs

getValueFunct1 env i = head [ val | (num, val) <- env, i == num ]

getValuesFunct nodes = (sort $ nub $ map ((\(i,_,_) -> i) . snd) nodes)

generateFunct [] = [[]]
generateFunct (x:xs) = ((map ((x, True):) (generateFunct xs) ++ map ((x, False):) (generateFunct xs)))


makeEmpty n = (n, ('0', n, n))
doGet :: NodeId -> [BDDNode] -> BDDNode
doGet i vars = if i > 1 then head [ x | x <- vars, fst x == i ] else makeEmpty i

-- Задача 3 -----------------------------------------
simplify :: BExp -> BExp
simplify (Not (Bvalue elx)) = Bvalue (not elx)
simplify (Or (Bvalue elx) (Bvalue ely)) = Bvalue (elx || ely)
simplify (And (Bvalue elx) (Bvalue ely)) = Bvalue (elx && ely)
simplify elx = elx	

-- Задача 4 -----------------------------------------
restrict :: BExp -> Char -> Bool -> BExp
restrict (Bvalue el) _ _ = (Bvalue el)
restrict (Bvar el) id bool
  | el == id   = (Bvalue bool)
  | otherwise = Bvar el
restrict (Not el) id bool = simplify (Not (restrict el id bool))
restrict (And elx ely) id bool = simplify (And (restrict elx id bool) (restrict ely id bool))
restrict (Or elx ely) id bool = simplify (Or (restrict elx id bool) (restrict ely id bool))
-- Задача 5 -----------------------------------------
-- Передумова: Кожна змінна (буква) в булевому виразі (BExp) з"являється 
--    точно один раз в списку змінних (Char); немає інших елементів
buildBDD :: BExp -> [Char] -> BDD
buildBDD elem ls = buildBDD' elem 2 ls 

buildBDD' :: BExp -> NodeId -> [Char] -> BDD
buildBDD' elem _ []
  | elem == Bvalue False = (0, [])
  | elem == Bvalue True  = (1, [])
buildBDD' elem i (x : ls)
  = (i, (i, (x, leftData, rightData)) : leftNodes ++ rightNodes)
  where 
    (leftData, leftNodes)   = buildBDD' (restrict elem x False) (2 * i) ls
    (rightData, rightNodes) = buildBDD' (restrict elem x True) (2 * i + 1) ls

-- Задача 6 -----------------------------------------
-- Передумова: Кожна змінна (буква) в булевому виразі (BExp) з"являється 
--    точно один раз в списку змінних (Char); немає інших елементів
buildROBDD :: BExp -> [Index] -> BDD
buildROBDD elem ls = buildROBDD' elem 2 ls
  
buildROBDD' :: BExp -> NodeId -> [Index] -> BDD
buildROBDD' elem _ []
  | elem == Bvalue False = (0, [])
  | elem == Bvalue True  = (1, [])
buildROBDD' elem n (l:ls)
  | leftId /= rightId = (n, (n, (l, leftId, rightId)) : leftNodes ++ rightNodes)
  | otherwise         = (n, leftNodes ++ rightNodes)
  where 
    (leftId, leftNodes)   = buildBDD' (restrict elem l False) (2 * n) ls
    (rightId, rightNodes) = buildBDD' (restrict elem l True) (2 * n + 1) ls

-- Задача 7 -----------------------------------------

------------------------------------------------------
-- Приклади для тестування..
bs1, bs2, bs3, bs4, bs5, bs6, bs7, bs8, bs9, bs10, bs11 :: String
bs1 = "F"
bs2 = "!(x&(F|y))"
bs3 = "u&T"
bs4 = "d&(x|!y)"
bs5 = "!(d&(x|!y))"
bs6 = "u&x|y&z" 
bs7 = "!y|(x|!e)"
bs8 = "u|!u"
bs9 = "z&(y|!y&x)"
bs10 = "d&(x|!y)"
bs11 = "!(d&(x|!y))"

b1, b2, b3, b4, b5, b6, b7, b8, b9,b10, b11  :: BExp
b1 = Bvalue False
b2 = Not (And (Bvar 'x') (Or (Bvalue False) (Bvar 'y')))
b3 = And (Bvar 'u') (Bvalue True)
b4 = And (Bvar 'd') (Or (Bvar 'x') (Not (Bvar 'y')))
b5 = Not (And (Bvar 'd') (Or (Bvar 'x') (Not (Bvar 'y'))))
b6 = Or (And (Bvar 'u') (Bvar 'x')) (And (Bvar 'y') (Bvar 'z'))
b7 = Or (Not (Bvar 'y')) (Or (Bvar 'x') (Not (Bvar 'e')))
b8 = Or (Bvar 'u') (Not (Bvar 'u'))
b9 = And (Bvar 'z') (Or (Bvar 'y') (And (Not (Bvar 'y')) (Bvar 'x')))
b10 = Not (And (Bvar 'x') (Or (Bvalue False) (Bvar 'y')))
b11 = And (Bvar 'l') (Bvalue False)

bdd1, bdd2, bdd3, bdd4, bdd5, bdd6, bdd7, bdd8, bdd9, bdd10, bdd11, bdd12 :: BDD
bdd1 = (0,[])
bdd2 = (2,[(2,('x',4,5)),(4,('y',1,1)),(5,('y',1,0))])
bdd3 = (5,[(5,('u',0,1))])
bdd4 = (2,[(2,('x',4,5)),(4,('y',8,9)),(8,('d',0,1)),(9,('d',0,0)),
           (5,('y',10,11)),(10,('d',0,1)),(11,('d',0,1))])
bdd5 = (3,[(4,('y',8,9)),(3,('x',4,5)),(8,('d',1,0)),(9,('d',1,1)),
           (5,('y',10,11)),(10,('d',1,0)),(11,('d',1,0))])
bdd6 = (2,[(2,('u',4,5)),(4,('x',8,9)),(8,('y',16,17)),(16,('z',0,0)),
           (17,('z',0,1)),(9,('y',18,19)),(18,('z',0,0)),(19,('z',0,1)),
           (5,('x',10,11)),(10,('y',20,21)),(20,('z',0,0)),(21,('z',0,1)),
           (11,('y',22,23)),(22,('z',1,1)),(23,('z',1,1))])
bdd7 = (6,[(6,('x',4,5)),(4,('y',8,9)),(8,('e',1,1)),(9,('e',1,0)),
           (5,('y',10,11)),(10,('e',1,1)),(11,('e',1,1))])
bdd8 = (2,[(2,('u',1,1))])
bdd9 = (2,[(2,('x',4,5)),(4,('y',8,9)),(8,('z',0,0)),(9,('z',0,1)),(5,('y',10,11)),(10,('z',0,1)),(11,('z',0,1))])
bdd10 = (6,[(6,('x',4,5)),(4,('y',8,9)),(8,('e',1,1)),(9,('e',1,0)),
           (5,('y',10,11)),(10,('e',1,1)),(11,('e',1,1))])
bdd11 = (2,[(2,('u',1,1))])
bdd12 = (2,[(2,('l',1,1))])


