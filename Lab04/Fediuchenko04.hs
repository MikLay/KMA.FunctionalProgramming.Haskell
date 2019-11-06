{-# OPTIONS_GHC -Wall #-}
module Fediuchenko04 where

type Graph  = [[Int]]

nodes::Graph -> [Int]
nodes g = [0..(length g)-1]

edgeIn::Graph -> (Int, Int) -> Bool
edgeIn g (x,y) = elem y (g!!x)

edges::Graph -> [(Int, Int)]
edges g = [(x,y)| x <- (nodes g), y <- (g!!x) ]

--  1 ------------------------------------
isGraph :: Graph -> Bool 
isGraph [] = True
isGraph (g:gr) = (isAllUnique g) && isGraph gr 

isAllUnique :: [Int] -> Bool
isAllUnique [] = True
isAllUnique (v:vs) = (notElem v vs) && isAllUnique vs 

--  2 ------------------------------------
isTournament :: Graph -> Bool 
isTournament gr = head (isEdges gr) && head (isEdges (tail gr));

isEdges:: Graph -> [Bool]
isEdges gr = [ haveEdge i j gr | i <- nodes gr, j <- nodes gr, i /= j]

haveEdge :: Int -> Int -> Graph -> Bool
haveEdge v w gr = edgeIn gr (v, w) || edgeIn gr (w, v)

--  3 ------------------------------------
isTransitive :: Graph -> Bool 
isTransitive gr =  verifyLinks (allLinks gr)

verifyLinks :: Foldable t => [t a] -> Bool
verifyLinks (l1:l2:[]) = True
verifyLinks ls = (length (head ls)) >= 2 && verifyLinks (tail ls)

allLinks gr = [ transPredicate e (edges gr) | e <- (edges gr)]
allLinks :: Graph -> [[(Int, Int)]]
transPredicate :: Eq b => (b, b) -> [(b, b)] -> [(b, b)]
transPredicate (v, u) es@(y:edges) = if (elem (v, u) es) && (elem (u, snd y) es)
                                    && (elem (v, snd y) es)
                                    then [(v, u), (u, snd y), (v, snd y)]
                                    else [(v, u)]
