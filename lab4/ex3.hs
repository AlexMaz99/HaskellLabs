-- Algebraiczne typy danych 2: rekursja strukturalna

data BinIntTree = EmptyIntBT | 
                  IntNodeBT Int BinIntTree BinIntTree

sumBinIntTree :: BinIntTree -> Int
sumBinIntTree EmptyIntBT = 0
sumBinIntTree (IntNodeBT n lt rt) = n + sumBinIntTree lt + sumBinIntTree rt

data BinTree a = EmptyBT |
                 NodeBT a (BinTree a) (BinTree a)
            
sumBinTree :: (Num a) => BinTree a -> a
sumBinTree EmptyBT = 0
sumBinTree (NodeBT n lt rt) = n + sumBinTree lt + sumBinTree rt

data Expr a = Lit a | -- literal/value a, e.g. Lit 2 = 2
              Add (Expr a) (Expr a) |
              Subtract (Expr a) (Expr a) |
              Product (Expr a) (Expr a)

eval :: Num a => Expr a -> a
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Subtract e1 e2) = eval e1 - eval e2
eval (Product e1 e2) = eval e1 * eval e2

show' :: Show a => Expr a -> String
show' (Lit n) = show n
show' (Add e1 e2) = "(" ++ show' e1 ++ "+" ++ show' e2 ++ ")"
show' (Subtract e1 e2) = "(" ++ show' e1 ++ "-" ++ show' e2 ++ ")"
show' (Product e1 e2) = "(" ++ show' e1 ++ "*" ++ show' e2 ++ ")"

depthOfBT :: BinTree a -> Int
depthOfBT EmptyBT = 0
depthOfBT (NodeBT _ lt rt) = 1 + max (depthOfBT lt) (depthOfBT rt)

preorderBT :: BinTree a -> [a]
preorderBT EmptyBT = []
preorderBT (NodeBT n lt rt) = n : preorderBT lt ++ preorderBT rt

inorderBT :: BinTree a -> [a]
inorderBT EmptyBT = []
inorderBT (NodeBT n rt lt) = inorderBT lt ++ [n] ++ inorderBT rt

postorderBT :: BinTree a -> [a]
postorderBT EmptyBT = []
postorderBT (NodeBT n rt lt) = postorderBT lt ++ postorderBT rt ++ [n]

mapBT :: (a -> b) -> BinTree a -> BinTree b
mapBT _ EmptyBT = EmptyBT
mapBT f (NodeBT n rt lt) = NodeBT (f n) (mapBT f lt) (mapBT f rt)

insert :: Ord a => a -> BinTree a -> BinTree a
insert x EmptyBT = NodeBT x EmptyBT EmptyBT
insert x (NodeBT n rt lt) 
    | x == n    = NodeBT n lt rt
    | x < n     = NodeBT n (insert x lt) rt
    | x > n     = NodeBT n lt (insert x rt)

occurs :: Ord a => a -> BinTree a -> Int
occurs _ EmptyBT = 0
occurs x (NodeBT n rt lt) = if x==n then 1 else 0 + occurs x lt + occurs x rt

elemOf :: Eq a => a -> BinTree a -> Bool
elemOf _ EmptyBT = False
elemOf x (NodeBT n rt lt) = x==n || elemOf x lt || elemOf x rt

minElemOf :: (Ord a, Fractional a) => BinTree a -> a
minElemOf EmptyBT = 1/0
minElemOf (NodeBT n rt lt) = min n (min (minElemOf lt) (minElemOf rt))

maxElemOf :: (Ord a, Fractional a) => BinTree a -> a
maxElemOf EmptyBT = -1/0
maxElemOf (NodeBT n rt lt) = max n (max (maxElemOf lt) (maxElemOf rt))

-- przykładowe drzewo let x = NodeBT 1 (NodeBT 2 EmptyBT EmptyBT) (NodeBT 3 EmptyBT EmptyBT)