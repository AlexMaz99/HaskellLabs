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

<<<<<<< HEAD
f9' = \x -> if x == 1 then 3 else 0
=======
f9' = \x -> if x == 1 then 3 else 0

--2. Funkcje wyższego rzędu: funkcje jako parametry/argumenty

sum' :: Num a => [a] -> a
sum' []     = 0
sum' (x:xs) = x + sum' xs

sumSqr' :: Num a => [a] -> a
sumSqr' []     = 0
>>>>>>> 358f5788d931f53a38d656cd5fb3eb1d6dbe505a
