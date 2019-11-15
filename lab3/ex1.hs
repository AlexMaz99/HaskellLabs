-- zagadnienia do kartkówki wzorzec collection pipeline, 
-- działanie funkcji wyższego rzędu
-- dwa foldy lewe i prawe czym się różnią
-- podane wyrażenie obliczyć wynik

-- druga grupa zadań
-- funkcje wyższego rzędu
-- napisać funkcje która zwróci inną funkcję jako wynik
-- która przyjmuje inną funkcję jako parametr
-- umieć napisać filter, map, redukcja, który będzie semantycznie prawidłowy
-- implementacja map dla list

-- notacje z wyrażeniami lamba
-- napisać funkcję anonimową
-- zapisz w postaci lamby (*)2 -> \x->x*2
-- (\x -> x +1.5) 1 jakiego typu będzie wynik

-- 1. Funkcje anonimowe i currying

f1 = \x -> x - 2

f2 = \x y -> sqrt (x^2 + y^2)

f3 = \x y z -> sqrt ((x)^2 + (y)^2 + (z)^2)

x1 = \x -> 2 * x

x2 = \x -> x * 2

x3 = \x -> 2 ^ x

x4 = \x -> x ^ 2

x5 = \x-> 2 / x

x6 = \x -> x / 3

x7 = \x -> 4 - x

f7 x = if x `mod` 2 == 0 then True else False
f7' = \x -> if x `mod` 2 == 0 then True else False

f8 x = let y = sqrt x in 2 * y^3 * (y + 1)
f8' = \x -> let y = sqrt x in 2 * y^3 * (y + 1)

f9 1 = 3
f9 _ = 0

f9' = \x -> if x == 1 then 3 else 0

--2. Funkcje wyższego rzędu: funkcje jako parametry/argumenty

sum' :: Num a => [a] -> a
sum' []     = 0
sum' (x:xs) = x + sum' xs

sumSqr' :: Num a => [a] -> a
sumSqr' []     = 0