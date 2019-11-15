not' :: Bool -> Bool
not' True = False
not' False = True

isItAnswer :: String -> Bool
isItAnswer "Love" = True
isItAnswer _ = False

or' :: (Bool, Bool) -> Bool
or' (False, False) = False
or' (_, _) = True

and' :: (Bool, Bool) -> Bool
and' (True, True) = True
and' (_, _) = False

nand' :: (Bool, Bool) -> Bool
nand' (x,y) = not' (and'(x,y))

xor' :: (Bool, Bool) -> Bool
xor' (True,True) = False
xor' (False,False) = False
xor' (_,_) = True 