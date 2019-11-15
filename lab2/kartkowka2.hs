-- 1. funkcja f=2* x przy użyciu sekcji

double_ :: Integer -> Integer
double_  = (*) 2

-- 2. sygnatura funkcji zipWith

--zipWith :: (a->b->c)->[a]->[b]->[c]

-- 3. sygnatura funkcji unzip

--unzip :: [(a,b)] -> ([a], [b])

-- 4. kwadraty parzystych liczb z przedziału [1..10] (list comprehension)

-- [x^2 | x<-[1..10], x `mod` 2 == 0]

-- 5. Iloczyn elementów listy przy użyciu rekursji ogonowej (akumulatora)

prod' :: Num a => [a] -> a
prod' = loop 1
    where loop acc []     = acc
          loop acc (x:xs) = loop (acc*x) xs

-- 6. Section dodający +3

_plus3 :: Num a => a -> a
_plus3 = (+) 3

-- 7. Sygnatura uncurry i curry

--uncurry :: (a -> b -> c) -> (a,b) -> c
--curry :: ((a,b) -> c) -> a -> b -> c

-- 8. Lista sześcianów nieparzystych liczb [1..10]

--[x^3 | x<-[1..10], x `mod` 2 /= 0]

--9. selectEven :: Integral a => [a] -> [a] rekursja ogonowa

selectEven :: Integral a => [a] -> [a]
selectEven = loop []
    where loop acc []       = acc
          loop acc (x:xs)   = if (x `mod` 2 == 0) then loop (acc ++ [x]) xs
                              else loop (acc) xs

-- 10. Co zwrócą poniższe list comprehensions:
-- a) [(3,j) | i<-[2,1], j<-[i..2]]   ->      [(3,2),(3,1),(3,2)]
-- b) [[i^2,j^2] | i<-[1..2], j<-[1..2], j>i]    ->    [[1,4]]

-- 11. Uzupełnić definicję i zrobić z tego rekurencję ogonową
-- sumAbs :: Num a => [a] -> a
-- sumAbs [] = _
-- sumAbs _  = _ _ sumAbs _

sumAbs :: (Num a) => [a] -> a
sumAbs [] = 0
sumAbs (x:xs)  = abs(x) + sumAbs xs

sumAbs' :: Num a => [a] -> a
sumAbs' = loop 0
    where loop acc []   = acc
          loop acc (x:xs) = loop ( abs(x) + acc) xs

-- 12. Co pokaże :t f 1 2
--     f1 :: Num t => t -> t -> (t,t) -> t

-- f1 :: Num t => (t,t) -> t