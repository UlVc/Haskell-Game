module ModificadorTexto where

 import Data.List.Split
 import Data.Char

 {-|
   Function that receives a string and removes spaces, points, capitals and adjectives.
   e.g.
   >>> modificarTexto "A star ....... is BELOW tHe hill..."
   "starisbelowhill"
-}
 modificarTexto :: String -> String
 modificarTexto s = filter (/=' ') $ elimina "the" $ elimina "a" $ elimina "an" $ filter (/= '.') s

 {-|
   Function that removes letters of a string.
   e.g.
   >>> elimina "awesome" "You are awesome"
   "you are "
-}
 elimina :: String -> String -> String
 elimina s c = map toLower $ unir $ elElemento s $ splitOn " " c

 {-|
   Function that removes an element of a list.
   e.g.
   >>> elElemento 4 [1,2,3,4,5]
   [1,2,3,5]
-}
 elElemento :: (Eq a) => a -> [a] -> [a]
 elElemento e [] = []
 elElemento e l = filter (\x -> x /= e) l

 {-|
   Function that concatenates the elements of a list using spaces.
   e.g.
   >>> unir ["My","name","is","Kakarotto"]
   "My name is Kakarotto "
-}
 unir :: [String] -> String
 unir [] = ""
 unir (x:xs) = x ++ " " ++ (unir xs)

 {-|
   Function that duplicates n times a string.
   e.g.
   >>> duplicar "Hi" 5
   "HiHiHiHiHi"
-}
 duplicar :: String -> Int -> String
 duplicar s n = concat $ replicate n s