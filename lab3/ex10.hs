-- Funkcje: zip, unzip i zipWith

isSortedAsc :: Ord a => [a] -> Bool
isSortedAsc xs = and $ zipWith (<=) xs (tail xs)

everySecond :: [a] -> [a]
everySecond [] = []
everySecond [x] = [x]
everySecond (x:y:xs) = x : everySecond xs

zip3' :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3' _ _ [] = []
zip3' _ [] _ = []
zip3' [] _ _ = []
zip3' (x:xs) (y:ys) (z:zs) = (x,y,z) : zip3' xs ys zs

isSortedDesc :: Ord a => [a] -> Bool
isSortedDesc xs = and $ zipWith (>=) xs (tail xs)

isSorted :: Ord a => [a] -> Bool
isSorted xs = isSortedAsc xs || isSortedDesc xs