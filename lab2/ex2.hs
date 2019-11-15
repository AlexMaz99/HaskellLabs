fact :: Integer -> Integer
fact n = if n==0 || n==1 then 1
        else n*fact (n-1)

fibb :: (Num a, Eq a) => a -> a
fibb n = if n ==0 || n==1 then n
        else fibb (n-2) + fibb (n-1)

sum1 :: Num a => [a] -> a
sum1 []      = 0
sum1 (x:xs)  = x + sum1 xs

length1 :: [a] -> Int
length1 []   = 0
length1 (_:xs) = 1 + length1 xs

qSort :: Ord a => [a] -> [a]
qSort []    = []
qSort (x:xs)= qSort (leftPart xs) ++ [x] ++ qSort (rightPart xs)
    where
     leftPart xs  = [y | y<-xs, y<=x]
     rightPart xs = [y | y<-xs, y>x]

fst2Eq :: Eq a => [a] -> Bool
fst2Eq (x : y : _) | x==y = True
fst2Eq _                  = False

prod' :: Num a => [a] -> a
prod' = loop 1 where
    loop acc []     = acc
    loop acc (x:xs) = loop (x * acc) xs