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

-- Exercise 3.7
extraParam :: (a -> b) -> (a -> c -> b)
extraParam f a b = f a

length'' :: [a] -> Integer
length'' xs = sum $ map (const 1) xs

length''' :: [a] -> Integer
length''' = foldl (extraParam $ (+) 1) 0

-- Exercise 3.8
doubleEach :: [Integer] -> [Integer]
doubleEach = map (* 2)

type IntegerPair = (Integer, Integer)

pairAndOne :: [Integer] -> [IntegerPair]
pairAndOne = map toPairAndOne
             where toPairAndOne x = (x, x + 1)

addEachPair :: [IntegerPair] -> [Integer]
addEachPair = map sumPair
              where sumPair (x, y) = x + y

addPairsPointwise :: [IntegerPair] -> IntegerPair
addPairsPointwise = foldr addPointwise (0, 0)
                    where addPointwise (x1, x2) (y1, y2) = (x1 + y1, x2 + y2)

-- Exercise 3.9
fuse :: [Dur] -> [Dur -> Music a] -> [Music a]
fuse [] [] = []
fuse _ [] = error "fuse: Lists have unequal length"
fuse [] _ = error "fuse: Lists have unequal length"
fuse (d:ds) (f:fs) = f d : fuse ds fs

-- Exercise 3.10
maxAbsPitch :: [AbsPitch] -> AbsPitch
maxAbsPitch [] = error "maxAbsPitch: Empty list"
maxAbsPitch xs = mAbs 0 xs
              where mAbs curr (x:xs) = if x > curr then mAbs x xs else mAbs curr xs
                    mAbs curr [] = curr

maxAbsPitch' :: [AbsPitch] -> AbsPitch
maxAbsPitch' = foldr1 greaterPitch
               where greaterPitch x y = if x > y then x else y

maxAbsPitch'' :: [AbsPitch] -> AbsPitch
maxAbsPitch'' = foldr1 max

minAbsPitch'' :: [AbsPitch] -> AbsPitch
minAbsPitch'' = foldr1 min

-- Exercise 3.11
-- |Chromatic scale between pitches p1 and p2, notes are separated by one semitone (one absolute pitch)
chrom :: Pitch -> Pitch -> Music Pitch 
chrom p1 p2 = foldr ((:+:) . note qn . pitch) (rest 0) absPitchRange
              where absPitches = (absPitch p1, absPitch p2)
                    rangeDirection = if uncurry (>) absPitches then -1 else if uncurry (==) absPitches then 0 else 1
                    absPitchRange = [fst absPitches, fst absPitches + rangeDirection..snd absPitches]