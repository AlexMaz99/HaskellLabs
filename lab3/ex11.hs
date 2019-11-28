-- Funkcje concat i concatMap

concat' :: [[a]] -> [a]
concat' []     = []
concat' (x:xs) = x ++ concat' xs

concat2 :: [[a]] -> [a]
concat2 xs = [y | x <- xs, y <- x]

concat3 :: [[a]] -> [a]
concat3 xs = foldr (++) [] xs