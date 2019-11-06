{-# OPTIONS_GHC -Wall #-}
module Fediuchenko08 where

import Data.Tree
data BTree a = BEmpty | BNode a (BTree a) (BTree a)
               deriving (Show, Eq)
-- Задача 1 -----------------------------------------
dfsForest ::  Forest a -> [a]
dfsForest [] = []
dfsForest (Node root []:ls) = root : dfsForest ls
dfsForest (Node root xs:ls) = root : (dfsForest xs) ++ dfsForest ls 

-- Задача 2 ----------------------------------------- 
bfsForest ::  Forest a -> [a]
bfsForest [] = []
bfsForest (Node root xs:ls) = root : (bfsForest $ ls ++ xs) 

-- Задача 3 -----------------------------------------
isInTree    :: (Eq a) => Tree a -> Tree a -> Bool
isInTree = (. subForest) . elem

-- Задача 4 -----------------------------------------
toBTree :: Forest a -> BTree a
toBTree [] = BEmpty
toBTree (Node root xs:ls) = BNode root (toBTree xs) (toBTree ls) 

-- Задача 5 -----------------------------------------
fromBTree :: BTree a -> Forest a  
fromBTree BEmpty = []
fromBTree (BNode key leftNode rightNode) = Node key (fromBTree leftNode) : (fromBTree rightNode) 

-- Задача 6 -----------------------------------------
isSearch :: (Ord a) => BTree a -> Bool
isSearch btr
    |isBTreeEmpty btr = True
    |(isBTreeEmpty $ pickLNode btr) && (isBTreeEmpty $ pickRNode btr) = True
    |(isBTreeEmpty $ pickLNode btr) = (isSearch $ pickLNode btr)&&(isSearch $ pickRNode btr)&&(pickKey btr < (pickKey $ pickRNode btr))
    |(isBTreeEmpty $ pickRNode btr) = (isSearch $ pickLNode btr)&&(isSearch $ pickRNode btr)&&(pickKey btr > (pickKey $ pickLNode btr))
    |otherwise = (isSearch $ pickLNode btr)&&(isSearch $ pickRNode btr)&&(pickKey btr > (pickKey $ pickLNode btr))&&(pickKey btr < (pickKey $ pickRNode btr))

pickKey::BTree a -> a
pickKey BEmpty = error "Root node is empty"
pickKey (BNode a _ _) = a

pickLNode::BTree a -> BTree a
pickLNode BEmpty = error "Left node is empty"
pickLNode (BNode _ x _) = x

pickRNode::BTree a -> BTree a
pickRNode BEmpty = error "Right node is empty"
pickRNode (BNode _ _ x) = x

isBTreeEmpty :: BTree a -> Bool
isBTreeEmpty BEmpty = True
isBTreeEmpty _ = False 

-- Задача 7  -----------------------------------------
elemSearch ::(Ord a) => BTree a -> a -> Bool
elemSearch btr key = elem key (orderNodes btr) 

orderNodes :: BTree a -> [a]
orderNodes BEmpty = []
orderNodes (BNode key leftNode rightNode) = (orderNodes leftNode)++[key]++(orderNodes rightNode)
-- Задача 8 ------------------------------------------
insSearch :: (Ord a) => BTree a -> a -> BTree a 
insSearch btr key 
    |(isBTreeEmpty btr) = (BNode key BEmpty BEmpty)
    |key>pickKey btr = (BNode (pickKey btr ) (pickLNode btr)  $ insSearch ( pickRNode btr) key)
    |key<=pickKey btr = (BNode (pickKey btr) (insSearch(pickLNode btr) key) $ pickRNode btr)
    |otherwise = btr

-- Задача 9 ------------------------------------------
delSearch :: (Ord a) => BTree a -> a -> BTree a
delSearch btr key
    |isBTreeEmpty btr  = btr
    |key<(pickKey btr)  = (BNode (pickKey btr) (delSearch (pickLNode btr) key) (pickRNode btr))
    |key>(pickKey btr) = (BNode (pickKey btr) (pickLNode btr) (delSearch (pickRNode btr) $ key))
    |key==(pickKey btr)&&isBTreeEmpty(pickLNode btr)&&(isBTreeEmpty $ pickRNode btr) = BEmpty
    |key==(pickKey btr)&&(isBTreeEmpty $ pickLNode btr) = pickRNode btr
    |key==(pickKey btr)&&(isBTreeEmpty $ pickRNode btr) = pickLNode btr
    |otherwise = (BNode (pickKey $ pickLNode btr ) (delSearch (pickLNode btr ) (pickKey $ pickLNode btr)) $ pickRNode btr) 

-- Задача 10 -----------------------------------------
sortList :: (Ord a) => [a] -> [a]
sortList [] = []
sortList ls = orderNodes (generateTree ls)	

generateTree :: (Ord a) => [a] -> BTree a
generateTree [] = BEmpty
generateTree ls = foldl (\tr root -> insSearch tr root) BEmpty ls

--- Впорядковані дерева
otx, subTree  :: [Tree Int]
otx =  [Node 1 [Node 2 [], 
                Node 3 [Node 10 []]] ,
        Node 4 [Node 5 [Node 8 []], 
                Node 6 [Node 9 []],
                Node 7 []] 
       ] 

subTree = [Node 2 [], 
                Node 3 [Node 10 []]]

---  Бінарні дерева 
btr, btr1, btr2, btr3 ::  BTree Int
btr = BNode 1 (BNode 2 BEmpty
                      (BNode 3 (BNode 10 BEmpty BEmpty)
                                BEmpty)
             ) 
             (BNode 4 (BNode 5 (BNode 8  BEmpty BEmpty)
                               (BNode 6  (BNode 9 BEmpty BEmpty)
                                         (BNode 7 BEmpty BEmpty)
                               )
                      )
              BEmpty
             )
btr1 = BNode 9 (BNode 4 BEmpty 
                       (BNode 8 BEmpty BEmpty))
              (BNode 20 (BNode 10 BEmpty BEmpty) 
                        BEmpty)

btr2 = BNode 9 (BNode 4 BEmpty 
                       (BNode 8 BEmpty BEmpty))
              (BNode 20 (BNode 10 BEmpty BEmpty) 
                        BEmpty)


btr3 = BNode 1 (BNode 2 BEmpty (BNode 3 (BNode 10 BEmpty BEmpty) BEmpty)) (BNode 4 (BNode 5 (BNode 8 BEmpty BEmpty) (BNode 6 (BNode 9 BEmpty BEmpty) (BNode 7 BEmpty BEmpty))) BEmpty)


