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

sumAbs :: (Ord a, Num a) => Tree a -> a
sumAbs (Leaf a) = a 
sumAbs (Node (lt:rt:xs) c) = abs c + abs (sumAbs lt) + abs (sumAbs rt)

-- przykładowe drzewo let x = Node [Node [Leaf 8, Leaf 4] 16, Leaf (-4)] (-1)