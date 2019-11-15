myFun x = 2 * x

--currying

add2T :: Num a => (a,a)->a
add2T (x,y) = x + y

add2C :: Num a => a -> (a -> a)
add2C x y = x + y

add3T :: Num a => (a,a,a) -> a
add3T (x,y,z) = x + y + z

add3C :: Num a => a -> (a -> (a -> a))
add3C x y z = x + y + z

-- sections

fiveToPower_ :: Integer -> Integer
fiveToPower_ = (5 ^)

_ToPower5 :: Num a => a -> a
_ToPower5 = flip (^) 5

subtrNFrom5 :: Num a => a -> a
subtrNFrom5 = (5 -)

subtr5From_ :: Num a => a -> a
subtr5From_ = flip (-) 5

--operacje na listach

isPalindrome :: [Char] -> Bool
isPalindrome s = if (reverse s == s) then True
                else False

-- list comprehensions

isPrime :: Integral t => t -> Bool
isPrime n = [i | i<-[2..n-1], n `mod` i == 0] == []

--rekursja 1

fib :: (Num a, Eq a) => a -> a
fib n = if n == 0 || n == 1 then n
        else fib(n-2) + fib(n-1)

sum' :: Num a => [a] -> a
sum' []     = 0
sum' (x:xs) = x + sum' xs

prod' :: Num a => [a] -> a
prod' []        = 1
prod' (x:xs)    = x * prod' xs 

length' :: [a] -> Int
length' []      = 0
length' (_:xs)  = 1 + length' xs

or' :: [Bool] -> Bool
or' []      = False
or' (x:xs)  = x || or' xs

and' :: [Bool] -> Bool
and' []     = False
and' [True] = True
and' (x:xs) = x && and' xs

elem' :: Eq a => a -> [a] -> Bool
elem' n []     = False
elem' n (x:xs)
    | n == x    = True
    | otherwise = elem' n xs
            
doubleAll :: Num t => [t] -> [t]
doubleAll []     = []
doubleAll (x:xs) = 2*x : doubleAll xs

double :: Num t => [t] -> [t]
double = loop []
    where loop acc []       = acc
          loop acc (x:xs)   = loop (acc ++ [2*x]) xs

squareAll :: Num t => [t] -> [t]
squareAll []     = []
squareAll (x:xs) = [x^2] ++ squareAll xs

selectEven :: Integral t => [t] -> [t]
selectEven []     = []
selectEven (x:xs) = if (x `mod` 2 == 0) then x : selectEven xs
                    else selectEven xs

--rekursja ogonowa, akumulator

sum'2 :: Num a => [a] -> a
sum'2 xs = loop 0 xs
    where loop acc []     = acc
          loop acc (x:xs) = loop (x+acc) xs

sum'3 :: Num a => [a] -> a
sum'3 = loop 0
    where loop acc []     = acc
          loop acc (x:xs) = loop (x+acc) xs

prod'2 :: Num a => [a] -> a
prod'2 = loop 1
    where loop acc []     = acc
          loop acc (x:xs) = loop(x*acc) xs

length'2 :: [a] -> Int
length'2 = loop 0
    where loop acc []     = acc
          loop acc (_:xs) = loop(1+acc) xs

-- rekursja 4

qSort :: Ord a => [a] -> [a]
qSort []     = []
qSort (x:xs) = qSort (leftPart xs) ++ [x] ++ qSort (rightPart xs)
    where leftPart xs = [y | y<-xs, y<=x]
          rightPart xs = [y | y<-xs, y>x]

qSort' :: Ord a => [a] -> [a]
qSort' []   = []
qSort' (x:xs) = qSort (leftPart xs) ++ [x] ++ qSort (rightPart xs)
    where leftPart xs  = filter (<=x) xs
          rightPart xs = filter (>x) xs

-- dopasowanie wzorców: guards

fst2Eq :: Eq a => [a] -> Bool
fst2Eq (x : y : _) | x == y = True
fst2Eq _                    = False

isDivide :: Integral a => [a] -> Bool
isDivide (x : _) | x == 0             = False
isDivide (x : y : _) | y `mod` x == 0 = True
isDivide _                            = False

isDivide3 :: Integral a => [a] -> Bool
isDivide3 (x : _) | x == 0                 = False
isDivide3 (x : _ : y : _) | y `mod` x == 0 = True
isDivide3 _                                = False