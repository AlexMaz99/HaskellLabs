-- Funkcje wyższego rzędu: map

import Data.Char

doubleElems [] = []
doubleElems (x:xs) = 2 * x : doubleElems xs

sqrElems [] = []
sqrElems (x:xs) = x^2 : sqrElems xs

lowerCase [] = []
lowerCase (x:xs) = toLower x : lowerCase xs

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

doubleElems' = map' (*2)
sqrElems' = map' (^2)
lowerCase' = map' toLower

doubleElems'' xs = [x * 2 | x <- xs]
sqrElems'' xs = [x^2 | x <- xs]
lowerCase'' xs = [toLower x | x <- xs]

f1 = length . filter even $ doubleElems [1..10^7]
f2 = length . filter even . map (*2) $ [1..10^7]
f3 = length . filter even $ [2*x | x <- [1..10^7]]