data Maybe a = Nothing | Just a

instance Monad Maybe where
    (Just x) >>= k = k x
    Nothing >>= _ = Nothing
    (>>) = (*>)
    fail _ = Nothing

join :: Maybe (Maybe a) -> Maybe a
join (Just (Just a)) = Just a
join (Just Nothing)  = Nothing
join Nothing         = Nothing

data [] a = [] | a : [a]

instance Monad [] where
    xs >>= f = [y | x <- xs, y <- f x]
    (>>) = (*>)
    fail _ = []

join :: [[a]] -> [a]
join []       = []
join (xs:xss) = xs ++ join xss

data Either e a = Left e | Right a

instance Monad (Either e) where
    return = Right
    Right x >>= f = f x
    Left err >>= _ = Left err

join :: Either e (Either e a) -> Either e a
join (Right (Right x))  = Right x
join (Right (Left err)) = Left err
join (Left err)         = Left err

instance Monad ((->) r) where
    f >>= k = \x -> k (f x) x

join :: (r -> (r -> a)) -> (r -> a)
join f = \x -> f x x