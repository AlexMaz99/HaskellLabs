(<$<) :: (a -> b) -> a -> b
(<$<) = ($)

(>$>) :: a -> (a -> b) -> b
x >$> f = f x
infixl 0 >$>

(<.<) :: (b -> c) -> (a -> b) -> (a -> c)
(<.<) = (.)

(>.>) :: (a -> b) -> (b -> c) -> (a -> c)
f >.> g = g . f
infixl 9 >.>

g1 = (+2) <.< (*3) <$< 5 -- 17 
g2 = (+2) >.> (*3) <$< 5 --21
g3 = 5 >$> (+2) <.< (*3) --17
g4 = 5 >$> (+2) >.> (*3) --21
g5 = 3 >$> (+2) >$> (+10) --15

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x:xs) = Just xs

extractMaybe :: Maybe a -> a
extractMaybe Nothing = error "Nothing inside!"
extractMaybe (Just x) = x

insertMaybe :: a -> Maybe a
insertMaybe = Just

-- (>$>) = extract (^) and apply ($)
--(>^$>) :: Maybe a -> (a -> Maybe b) -> Maybe b
--ma >^$> f = (extractMaybe ma) >$> f
--infixl 1 >^$>

(>^$>) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing >^$> _ = Nothing
(Just x) >^$> f = f x
infixl 1 >^$>

f1 :: (Ord a, Num a) => a -> Maybe a
f1 x = if x > 0 then Just (10 * x) else Nothing

f2 :: (Eq a, Num a) => a -> Maybe a
f2 x = if x/= 0 then Just (10 * x) else Nothing

-- Kleisli composition
(>.>>) :: (a -> Maybe b) -> (b -> Maybe c) -> (a -> Maybe c)
f >.>> g = \x -> g (extractMaybe (f x))