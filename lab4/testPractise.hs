data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)

surface :: Shape -> Float
surface (Circle _ _ r) = pi * r^2
surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)

data Point = Point Float Float deriving (Show)
data Shape' = Circle' Point Float | Rectangle' Point Point deriving (Show)

surface' :: Shape' -> Float
surface' (Circle' _ r) = pi * r^2
surface' (Rectangle' (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

-- record syntax

data Person = MkPerson { firstName :: String 
                         , lastName :: String
                         , age :: Int
                         }deriving (Show)

data Foo a = MkFoo { value :: a
                    , name :: String}
instance Show a => Show (Foo a) where
    show MkFoo{ value = v, name = n } = "Name: " ++ n ++ " with " ++ show v

data Tree a = Node [Tree a] a | 
              Leaf a

sumTree :: (Ord a, Num a) => Tree a -> a
sumTree (Leaf a) = a 
sumTree (Node (lt:rt:xs) c) = c + sumTree lt + sumTree rt

productTree :: (Ord a, Num a) => Tree a -> a
productTree (Leaf a) = a 
productTree (Node (lt:rt:xs) c) = c * sumTree lt * sumTree rt

maxValue :: (Ord a) => Tree a -> a
maxValue (Leaf a) = a
maxValue (Node (lt:rt:xs) c) = max c (max (maxValue lt) (maxValue rt)) 

minValue :: (Ord a) => Tree a -> a
minValue (Leaf a) = a
minValue (Node (lt:rt:xs) c) = min c (min (minValue lt) (minValue rt)) 

sumAbs :: (Ord a, Num a) => Tree a -> a
sumAbs (Leaf a) = a 
sumAbs (Node (lt:rt:xs) c) = abs c + abs (sumAbs lt) + abs (sumAbs rt)

depth :: (Ord a) => Tree a -> Int
depth (Leaf a) = 1
depth (Node (lt:rt:xs) _) = 1 + max (depth lt) (depth rt)

elemOf :: (Eq a) => a -> Tree a -> Bool
elemOf x (Leaf a) = if x==a then True else False
elemOf x (Node (lt:rt:xs) a) = x==a || (elemOf x lt) || (elemOf x rt)

occurs :: Eq a => a -> Tree a -> Int
occurs x (Leaf a) = if x==a then 1 else 0
occurs x (Node (lt:rt:xs) a) = (if x==a then 1 else 0) + occurs x lt + occurs x rt

-- przykładowe drzewo let x = Node [Node [Leaf 8, Leaf 4] 16, Leaf (-4)] (-1)

data MyType = C1 (Bool, Int) | C2 Int | C3 Double

instance Eq MyType where
    (==) (C1 (x1, y1)) (C1 (x2, y2)) = x1==x2 && y1==y2
    (==) (C2 i1) (C2 i2) = i1 == i2
    (==) (C3 i1) (C3 i2) = i1 == i2

instance Show MyType where
    show (C1 (x, y)) = "C1 " ++ "("++ show x ++ "," ++ show y ++ ")"
    show (C2 x) = "C2 " ++ show x  
    show (C3 x) = "C3" ++ show x

data BinTree a = NodeBT (BinTree a) (BinTree a) a | Leaf' a

mapBT :: (a -> b) -> BinTree a -> BinTree b
mapBT f (Leaf' a) = Leaf' (f a)
mapBT f (NodeBT lt rt n) = NodeBT (mapBT f lt) (mapBT f rt) (f n)

depthOfBT :: BinTree a -> Int
depthOfBT (Leaf' _) = 1
depthOfBT (NodeBT lt rt _) = 1 + max (depthOfBT lt) (depthOfBT rt)