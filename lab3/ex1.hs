-- Funkcje anonimowe i currying

f1 = \x -> x - 2

f2 = \x y -> sqrt (x^2 + y^2)

f3 = \x y z -> sqrt ((x)^2 + (y)^2 + (z)^2)

x1 = \x -> 2 * x

x2 = \x -> x * 2

x3 = \x -> 2 ^ x

x4 = \x -> x ^ 2

x5 = \x-> 2 / x

x6 = \x -> x / 3

x7 = \x -> 4 - x

sqrt' = \x -> sqrt x
abs' = \x -> if x >=0 then x else (-x)
log' = \x -> log x
id' = \x -> x
const' = \x y -> const x y

f7 x = if x `mod` 2 == 0 then True else False
f7' = \x -> if x `mod` 2 == 0 then True else False

f8 x = let y = sqrt x in 2 * y^3 * (y + 1)
f8' = \x -> let y = sqrt x in 2 * y^3 * (y + 1)

f9 1 = 3
f9 _ = 0

f9' = \x -> if x == 1 then 3 else 0