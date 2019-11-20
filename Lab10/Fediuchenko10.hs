{-# OPTIONS_GHC -Wall #-}
module Fediuchenko10 where

import Data.Char(isSpace, isDigit, isLetter) 

type Name       = String
type Attributes = [(String, String)]
data XML        =  Text String | Element Name Attributes [XML] deriving (Eq, Show)


cV :: Char -> Bool
cV c = if (notElem c "\"") then True
       else False

sN :: Char -> Bool
sN oneC = if (isDigit oneC || isLetter oneC || oneC == '.' || oneC  == '-') then True
       else False

sT :: Char -> Bool
sT ls = if (notElem ls "<>") then True
       else False


-- Задача 1 -----------------------------------------
spaces :: String -> String
spaces [] = []
spaces (e:st) | isSpace e = spaces st
              | otherwise = e:st
  
-- Задача 2 ----------------------------------------- 
manyT, value, manyN :: String -> (String,String)

manyT str = 
  let pref = prfx sT str 
  in (pref, drop (length pref) str )

manyN str = 
  let pref = prfx sN str
  in (pref, drop (length pref) str )

value str = 
  let pref = prfx cV str
  in (pref, drop (length pref) str )


prfx :: (Char -> Bool) -> String -> String
prfx _ "" = ""
prfx func (st:str) | func st = st:(prfx func str)
              | otherwise = ""


-- Задача 3 -----------------------------------------
name, text, fullValue :: String ->  Maybe(String,String) 


name "" = Nothing
name str@(el:_) = if (isLetter el) then Just (fst (manyN str), snd (manyN str))
                  else Nothing

text "" = Just ("", "")
text str@(el:_) = if (sT el) then Just (fst (manyT str), snd ( manyT str))
                  else Nothing

fullValue "" = Nothing
fullValue (el:str) = if (el /= '\"') || (findQuote str 0) < 0 then Nothing 
                   else Just (fst (value str), drop 1 ( snd ( value str)))

findQuote :: [Char] -> Int -> Int
findQuote "" _ = -1
findQuote (el:str) m | el == '\"' = m
                    | otherwise = findQuote str (m+1)

dropQuotes :: String -> String
dropQuotes "" = ""
dropQuotes (_:str) = take (findQuote str 0) str

-- Задача 4 -----------------------------------------
-- Getters needed parts -----------------------------
getFirst, getSecond :: Maybe(String, String) -> String
getFirst Nothing = ""
getFirst (Just(first, _)) = first

getSecond Nothing = ""
getSecond (Just(_, second)) = second
getSecondDbl :: Maybe((String, String), String) -> String
getSecondDbl Nothing = ""
getSecondDbl (Just(_, second)) = second

getFirstDbl :: Maybe((String, String), String) -> (String, String)
getFirstDbl Nothing = ("", "")
getFirstDbl (Just(first, _)) = first

getFirstAtr :: Maybe ((String,Attributes),String) -> (String,Attributes)
getFirstAtr Nothing = ("", [])
getFirstAtr (Just(first, _)) = first

attrib :: String -> Maybe ((String,String),String) 
attrib lis = findls lis

manyAtt :: String -> Maybe (Attributes,String)
manyAtt lis = transfAtr lis []

findls :: String -> Maybe ((String,String),String)
findls xs | name xs /= Nothing = transfTwo (spaces $ getSecond (name xs)) (getFirst (name xs))
             | otherwise = Nothing

transfTwo :: String -> String -> Maybe ((String,String),String)
transfTwo [] _ = Nothing
transfTwo (el:ls) nam | el == '='  = transfSpaces (spaces ls) nam
                     | otherwise = Nothing

transfSpaces :: String -> String -> Maybe ((String,String),String)
transfSpaces ls nam | fullValue ls /= Nothing = Just((nam, getFirst (fullValue ls)), getSecond (fullValue ls))
                 | otherwise = Nothing

transfAtr :: String -> Attributes -> Maybe (Attributes,String)
transfAtr lx ly | attrib lx /= Nothing = transfAtr (getSecondDbl (attrib lx)) (ly ++ [getFirstDbl ( attrib lx)])
               | ly /= [] = Just (ly, lx)
               | otherwise = Nothing

-- Задача 5 -----------------------------------------
begTag :: String -> Maybe ((String,Attributes),String)
begTag "" = Nothing
begTag (el:str) = if el /= '<' then Nothing 
                else case name str of
                     Nothing -> Nothing
                     Just (n, rst1) -> case manyAtt rst1 of
                                       Just (as, ('>':rst2)) -> Just ((n, as), rst2)
                                       _ -> Nothing

endTag :: String -> Maybe (String,String) 
endTag (a1:a2:st) = if a1 /= '<' && a2 /= '/' then Nothing 
                    else case name st of
                         Just (n, (r:rst1)) -> if r == '>' 
                                               then Just (n, rst1)
                                               else Nothing
                         _ -> Nothing 
endTag _ = Nothing

-- Задача 6 -----------------------------------------
element :: String -> Maybe (XML,String)
element xs = getXMLs xs

xml :: String -> Maybe (XML,String)
xml st = case element st of
         Just (xs, rst1) -> Just (xs, rst1)
         Nothing -> case text st of
                    Just (tx, rst2) -> Just (Text tx, rst2)
                    Nothing -> Nothing 

xmls :: String -> [(XML, String)]
xmls "" = [((Text ""), "")]
xmls str = case xml str of
          Just (x, rst) -> (x, rst):(xmls rst)
          Nothing -> []

manyXML :: String -> Maybe ([XML],String)
manyXML str = if null xs 
             then Just ([], str) 
             else Just (map fst xs, last ( map snd xs)) where xs = xmls str 



getXMLs :: String -> Maybe (XML,String)
getXMLs xs | begTag xs /= Nothing = Just ((Element (fst (getFirstAtr ( begTag xs))) (snd (getFirstAtr (begTag xs))) []), "")
             | otherwise = Nothing
-- Задача 7 -----------------------------------------
fullXML :: String -> Maybe XML 
fullXML s = case element (spaces s) of  
            Just (xm,s1) -> if null (spaces s1) then Just xm else Nothing 
            Nothing      -> Nothing    

-- Тестові дані -------------------------------------------
-- Прості тести XML-об'єктів (без проміжків)
stst1, stst2, stst3 :: String
stst1 = "<a>A</a>"
stst2 = "<a x=\"1\"><b>A</b><b>B</b></a>"
stst3 = "<a>\
      \<b>\
        \<c att=\"att1\">text1</c>\
        \<c att=\"att2\">text2</c>\
      \</b>\
      \<b>\
        \<c att=\"att3\">text3</c>\
        \<d>text4</d>\
      \</b>\
    \</a>" 

-- Результати аналізу попередніх XML-об'єктів
x1, x2, x3, x4 , x5 :: XML
x1 = Element "a" [] [Text "A"]
x2 = Element "a"
            [("x","1")]
            [Element "b" [] [Text "A"],
             Element "b" [] [Text "B"]]
x3 = Element "a" 
            [] 
            [Element "b" 
                     [] 
                     [Element "c"
                              [("att","att1")] 
                              [Text "text1"],
                      Element "c" 
                              [("att","att2")]
                              [Text "text2"]],
             Element "b" 
                     [] 
                     [Element "c" 
                              [("att","att3")] 
                              [Text "text3"],
                      Element "d" 
                              [] 
                              [Text "text4"]]]


x4 = Element "c"
            [("c","2")]
            [Element "f" [] [Text "F"],
             Element "l" [] [Text "L"]]

x5 = Element "a" 
            [] 
            [Element "b" 
                     [] 
                     [Element "c"
                              [("first","second")] 
                              [Text "first"],
                      Element "c" 
                              [("same","samesecond")]
                              [Text "text2"]],
             Element "b" 
                     [] 
                     [Element "c" 
                              [("test1","test3")] 
                              [Text "text3"],
                      Element "d" 
                              [] 
                              [Text "text4"]]]
casablanca :: String 
casablanca
  = "<film title=\"Casablanca\">\n  <director>Michael Curtiz</director>\n  <year>1942\
    \</year>\n</film>\n\n\n"

casablancaParsed :: XML 
casablancaParsed
  = Element "film" 
            [("title","Casablanca")] 
            [Text "\n  ",
             Element "director" [] [Text "Michael Curtiz"],
             Text "\n  ",
             Element "year" [] [Text "1942"],
             Text "\n"]



