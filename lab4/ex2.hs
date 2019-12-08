-- 2. Algebraiczne typy danych 1: product & sum types, record syntax

-- product type example (one constructor)
data CartInt2DVec = MkCartInt2DVec Int Int -- konwencja: prefix 'Mk' dla konstruktora

xCoord :: CartInt2DVec -> Int
xCoord (MkCartInt2DVec x _) = x

yCoord :: CartInt2DVec -> Int
yCoord (MkCartInt2DVec _ y) = y

data Cart2DVec' a = MkCart2DVec' a a

xCoord' :: Cart2DVec' a -> a
xCoord' (MkCart2DVec' x _) = x

yCoord' :: Cart2DVec' a -> a
yCoord' (MkCart2DVec' _ y) = y

data Cart2DVec'' a = MkCart2DVec'' {x::a, y::a}

-- sum type example (two constructors)
data List a = EmptyL | Cons a (List a) deriving Show

head' :: List a -> a
head' EmptyL      = error "head': the empty list has no head!"
head' (Cons x xs) = x

-- enum type example (special case of sum type)
data ThreeColors = Blue |
                   White |
                   Yellow
type ActorName = String

leadingActor :: ThreeColors -> ActorName
leadingActor Blue = "Juliette Binoche"
leadingActor White = "Zbigniew Zamachowski"
leadingActor Yellow = "Irene Jacob"

data Cart3DVec a = Cart3DVec a a a
xCoord3 :: Cart3DVec a -> a
xCoord3 (Cart3DVec x _ _) = x

yCoord3 :: Cart3DVec a -> a
yCoord3 (Cart3DVec _ y _) = y

zCoord3 :: Cart3DVec a -> a
zCoord3 (Cart3DVec _ _ z) = z

data Cart3DVec' a = Cart3DVec' { x3 :: a
                               , y3 :: a
                               , z3 :: a
                               }

data Shape = Circle Float | Rectangle Float Float                              
area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rectangle x y) = x * y

data TrafficLights = Red | Orange | Green
actionFor :: TrafficLights -> String
actionFor Red = "Wait"
actionFor Orange = "Prepair to drive"
actionFor Green = "Drive"