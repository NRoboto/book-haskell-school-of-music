import Euterpea;

length' :: [a] -> Integer
length' [] = 0
length' (_:xs) = 1 + length' xs

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs

toAbsPitch :: [Pitch] -> [AbsPitch]
toAbsPitch = map' absPitch

-- Exercise 3.1
f1 :: Int -> [Pitch] -> [Pitch]
f1 shift = map $ trans shift

f2 :: [Dur] -> [Music a]
f2 = map rest

f3 :: [Music Pitch] -> [Music Pitch]
f3 [] = []
f3 (Prim (Note dur p):xs) = (note (dur/2) p :+: rest (dur/2)) : f3 xs
f3 (Prim (Rest dur):xs) = rest dur : f3 xs
-- End exercise

