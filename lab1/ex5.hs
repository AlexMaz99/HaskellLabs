sqn :: Int->Int
sqn n = if n<0 then -1 else if n==0 then 0 else 1

absInt :: Int -> Int
absInt n = if n<0 then -n else n

min2Int :: (Int, Int) -> Int
min2Int (x,y) = if x>y then y else x

min3Int :: (Int, Int, Int) -> Int
min3Int (x,y,z)=min2Int (min2Int (x,y),z)

