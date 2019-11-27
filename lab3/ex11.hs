-- Funkcje concat i concatMap

concat' :: [[a]] -> [a]
concat' []     = []
concat' (x:xs) = x ++ concat' xs