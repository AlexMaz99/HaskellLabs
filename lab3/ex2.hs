--2. Funkcje wyższego rzędu: funkcje jako parametry/argumenty

sum' :: Num a => [a] -> a
sum' []     = 0
sum' (x:xs) = x + sum' xs

sumSqr' :: Num a => [a] -> a
sumSqr' []     = 0
sumSqr' (x:xs) = x ^ 2 + sumSqr' xs

sumWith' :: Num a => (a -> a) -> [a] -> a
sumWith' f []     = 0
sumWith' f (x:xs) = f x + sumWith' f xs

sum'' x = sumWith' (\a -> a) x
sumSqr1 x = sumWith' (\a -> a^2) x
sumSqr2 = sumWith' (^2)
sumCube1 x = sumWith' (\a -> a^3) x
sumCube2 = sumWith' (^3)
sumAbs1 x = sumWith' (\a -> abs(a)) x
sumAbs2 = sumWith' abs

listLength = sumWith' (\a -> 1)

prod' :: Num a => [a] -> a
prod' []     = 1
prod' (x:xs) = x * prod' xs

prodWith :: Num a => (a -> a) -> [a] -> a
prodWith _ []     = 1
prodWith f (x:xs) = f x * prodWith f xs

prod = prodWith id
prodSqr = prodWith (^2)
prodCube = prodWith (^3)
prodAbs = prodWith abs

sumSqrt = sumWith' sqrt
prodSqrt = prodWith sqrt