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

(+|+) :: [a] -> [a] -> [a]
(+|+) [] ys = ys
(+|+) (x:xs) ys = x : (xs +|+ ys)

(!!!) :: Pitch -> Pitch -> Pitch 
(!!!) = max

fold' :: (a -> b -> b) -> b -> [a] -> b
fold' op init [] = init
fold' op init (x:xs) = x `op` fold' op init xs

line, chord :: [Music a] -> Music a
line = fold' (:+:) (rest 0)
chord = fold' (:=:) (rest 0)

maxPitch :: [Pitch] -> Pitch
maxPitch = fold' (!!!) (pitch 0)

maxPitch1 :: [Pitch] -> Pitch
maxPitch1 = foldr1 (!!!)

reverse' :: [a] -> [a]
reverse' xs = let rev acc [] = acc
                  rev acc (x:xs) = rev (x:acc) xs
              in rev [] xs

reverse'' :: [a] -> [a]
reverse'' = foldl (flip (:)) []

-- Exercise 3.4
applyEach :: [a -> b] -> a -> [b]
applyEach fs a = foldr applyOp [] fs
                 where applyOp f xs = f a : xs

-- Exercise 3.5
applyAll :: [a -> a] -> a -> a
applyAll = flip $ foldl $ flip ($)