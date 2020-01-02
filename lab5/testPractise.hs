-- Functor is a kind o functional representation of different Types which can be mapped over.
-- Functor is a function which takes a function, say, fmap() and return another function.

import Control.Applicative

x = foldr (+) 0 ((*) <$> ZipList[1,2,3] <*> ( (+1) <$> ZipList [4,5,6]))
-- foldr (+) 0 ZipList {getZipList = [5,12,21]} = 38

doFun = do
    putStrLn "Podaj imie: "
    s <- getLine
    putStrLn $ "Witaj " ++ s

fun = putStrLn "Podaj imie: " >> getLine >>= \s -> putStrLn $ "Witaj " ++ s

data Tree a = Node (Tree a) (Tree a) | Leaf a

instance Functor Tree where
    fmap f (Leaf a) = Leaf (f a)
    fmap f (Node lt rt) = Node (fmap f lt) (fmap f rt)

data Tree2 a = Node2 a (Tree2 a) (Tree2 a) | Leaf2

instance Functor Tree2 where
    fmap _ Leaf2 = Leaf2
    fmap f (Node2 a lt rt) = Node2 (f a) (fmap f lt) (fmap f rt)