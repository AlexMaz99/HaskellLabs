-- Funkcje: zip, unzip i zipWith

isSortedAsc :: Ord a => [a] -> Bool
isSortedAsc xs = and $ zipWith (<=) xs (tail xs)
