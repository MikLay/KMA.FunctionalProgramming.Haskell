{-# OPTIONS_GHC -Wall #-}
module HWI09 where

import Data.List hiding (insert, partition)

data SuffixTree = Leaf Int | Node [(String, SuffixTree)] 
                deriving (Eq, Ord, Show)

-- Задача 1 -----------------------------------------
isPrefix :: String -> String -> Bool
isPrefix [] _ = True
isPrefix _ [] = False
isPrefix (x:xs) (y:ys) = (x == y) && isPrefix xs ys 

-- Задача 2 -----------------------------------------
partition :: Eq a => [a] -> [a] -> ([a], [a], [a])
partition (x : xs) (y : ys)
  | x == y = (x : first, second, third)
  where
    (first, second, third) = partition xs ys
partition xs ys  = ([], xs, ys)

removePrefix :: String -> String -> String
removePrefix = drop . length

-- Задача 3 -----------------------------------------
suffixes :: [a] -> [[a]]
suffixes s@ (_:xs) = s : suffixes xs
suffixes _ = []

-- Задача 4 -----------------------------------------
isSubstring :: String -> String -> Bool
isSubstring = (. suffixes) . any . isPrefix

-- Задача 5 -----------------------------------------
findSubstrings :: String -> String -> [Int]
findSubstrings = (. suffixes) . findIndices . isPrefix


-- Задача 6 -----------------------------------------
getIndices :: SuffixTree -> [Int]
getIndices (Leaf leaf) = [leaf]
getIndices (Node node) = sort  $ concatMap (getIndices . snd) node





-- Задача 7 -----------------------------------------
findSubstrings' :: String -> SuffixTree -> [Int]
findSubstrings' "" (Leaf leaf) = [leaf]
findSubstrings' str (Node ((str2, ra) : pos))
  | null r1  = getIndices ra
  | null r1' = findSubstrings' r1 ra
  | otherwise  = findSubstrings' str $ Node pos
  where
    (_, r1, r1') = partition str str2
findSubstrings' _ _ = []

-- Задача 8 -----------------------------------------
connectTrees :: SuffixTree -> SuffixTree -> SuffixTree
connectTrees (Node node1) (Node node2) = Node $ node1 ++ node2

insert :: (String, Int) -> SuffixTree -> SuffixTree
insert (str, i) (Node []) = Node [(str, Leaf i)]
insert (str, i) (Node ((dat, t) : ats))
  | null pre  = connectTrees (Node [(dat, t)]) (insert (str, i) (Node ats))
  | pre == dat  = Node ((dat, insert (r1, i) t) : ats)
  | otherwise = connectTrees (Node [(pre, Node [(r1, Leaf i), (remA, t)])]) (Node ats)
  where
    (pre, r1, remA) = partition str dat


-- Ця функція задана
buildTree :: String -> SuffixTree 
buildTree s = foldl (flip insert) (Node []) (zip (suffixes s) [0..])

-- Задача 9 -----------------------------------------
longestRepeatedSubstring :: SuffixTree -> String
longestRepeatedSubstring = head . sortOn (negate . length) . substringss

isRepeatedSuffixTr :: SuffixTree -> Bool
isRepeatedSuffixTr (Node nod)
  | length nod > 1 = True
  | otherwise     = False


-- Find all repeated substrings
substringss :: SuffixTree -> [String]
substringss (Leaf _) = []
substringss (Node []) = [[]]
substringss (Node ((str, i@(Node _)) : pst))
  | isRepeatedSuffixTr i = map (str ++) (substringss i) ++ substringss (Node pst)
  | otherwise    = []
substringss (Node ((_, Leaf _) : pst)) = substringss (Node pst)



------------------------------------------------------
-- Приклади рядків і суфіксних дерев..

s1, s2, s3, s4, s5 :: String
s1 = "banana"
s2 = "mississippi"
s3 = "haskell"
s4 = "Hheck"
s5 = "Hh"



t1 :: SuffixTree
t1 = Node [("banana", Leaf 0), 
          ("a", Node [("na", Node [("na", Leaf 1), 
                                   ("", Leaf 3)]), 
                     ("", Leaf 5)]), 
          ("na", Node [("na", Leaf 2), 
                       ("", Leaf 4)])]

t2 :: SuffixTree
t2 = Node [("mississippi", Leaf 0), 
          ("i", Node [("ssi", Node [("ssippi", Leaf 1), 
                                    ("ppi", Leaf 4)]), 
                      ("ppi", Leaf 7), 
                      ("", Leaf 10)]), 
          ("s", Node [("si", Node [("ssippi", Leaf 2), 
                                   ("ppi", Leaf 5)]), 
                      ("i", Node [("ssippi", Leaf 3), 
                                  ("ppi", Leaf 6)])]), 
          ("p", Node [("pi", Leaf 8), 
                      ("i", Leaf 9)])]

-- create copy of t2
t3 :: SuffixTree
t3 = Node [("mississippi", Leaf 0), 
          ("i", Node [("ssi", Node [("ssippi", Leaf 1), 
                                    ("ppi", Leaf 4)]), 
                      ("ppi", Leaf 7), 
                      ("", Leaf 10)]), 
          ("s", Node [("si", Node [("ssippi", Leaf 2), 
                                   ("ppi", Leaf 5)]), 
                      ("i", Node [("ssippi", Leaf 3), 
                                  ("ppi", Leaf 6)])]), 
          ("p", Node [("pi", Leaf 8), 
                      ("i", Leaf 9)])]

