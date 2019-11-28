-- Wzorzec Collection pipeline

import Data.Char
import Data.List

capitalize :: [Char] -> [Char]
capitalize [] = []
capitalize (x:xs) = toUpper x : (map toLower xs)

formatStr s = foldr1 (\w s -> w ++ " " ++ s) . 
    map capitalize . 
    filter (\x -> length x > 1) $ 
    words s

prodPrices p = case p of
    "A" -> 100
    "B" -> 500
    "C" -> 1000
    _   -> error "Unknown product"

products = ["A","B","C"]

-- basic discount strategy
discStr1 p
    | price > 999 = 0.3 * price
    | otherwise   = 0.1 * price
    where price = prodPrices p

-- flat discount strategy
discStr2 p = 0.2 * prodPrices p

totalDiscount discStr =
    foldl1 (+) .
    map discStr .
    filter (\p -> prodPrices p > 499)

f1 = replicate 2 . product . map (*3) $ zipWith max [4,2] [1,5] -- wynik [180,180]

f2 = sum . takeWhile (<1000) . filter odd . map (^2) $ [1..] --wynik 5456