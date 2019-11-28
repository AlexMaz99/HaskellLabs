import Data.Char
import Data.List

-- 1. Funkcja dodająca do stringa "!"
f = \x -> x ++ "!"

-- 2. Funkcja mnożąca liczbę przez 5
fiveTimes = \x -> 5 * x

-- 3.
fun :: String -> Int
fun = sum .
    map ( foldl1 (\x y -> 2 * x + y)) .
    map (map digitToInt) . 
    filter (all (`elem` "01")) .
    words

funn = fun "To 100 zdanie nie10 ma 1010 11 sensu"
    -- wynik ["100", "1010", "11"] -> [[1,0,0],[1,0,1,0],[1,1]] -> [4,10,3] -> 17

fun2 :: String -> Int
fun2 = sum . map ((^2) . length) . filter (isUpper . head) . words

funn2 = fun2 "Ala posiaDa Kota a NIE psa"
    -- wynik ["Ala", "Kota", "NIE"] -> [9,16,9] -> 34

-- 4. 
g = sum [x^3 | x <- [1..10], x `mod` 3 == 0]
g2 = foldr (+) 0 $ map (^3) $ filter (\x -> x `mod` 3 == 0) [1..10]

h = product [x + 2 | x <- [1..10], odd x]
h2 = product $ map (+2) $ filter odd [1..10]
h3 = foldr (*) 1 . map (+2) $ filter odd [1..10]


-- 5. Zapisz za pomocą wyrażeń lambda
f1 x = let y = sqrt x in 2 * y^5 * (y+1)
f1' = \x -> let y = sqrt x in 2 * y^5 * (y+1)

-- 6. Co zwrócą funkcje (razem z kolejnymi etapami)
j = foldr1 (+) . map (*2) . filter (>99) $ [100,10,1000,500,90]
    -- [100,1000,500] -> [200,2000,1000] -> 3200
    
j' = foldr1 (++) . filter (\x -> length x <= 3) . map show $ [10^n | n <- [1..5]]
    -- ["10","100","1000","10000","100000"] -> ["10","100"] -> "10100"

-- 7. Napisz prodWith używając rekurencji, list comprehension, foldr1
prodWith :: Num a => (a -> a) -> [a] -> a
prodWith _ []     = 1;
prodWith f (x:xs) = f x * prodWith f xs

prodWith' f xs = product [f x | x <- xs]

prodWith'' f = foldr (\x acc -> f x * acc) 1