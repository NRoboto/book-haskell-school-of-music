import Euterpea

-- Exercise 5.1
twice :: (a -> a) -> (a -> a)
twice f = f . f

-- Exercise 5.2
power :: (a -> a) -> Integer -> (a -> a)
power f 0 = id
power f n = f . power f (n - 1)

-- Exercise 5.3
fix :: (a -> a) -> a
fix f = f (fix f)

remainder :: Integer -> Integer -> Integer
remainder = fix $ \r a b -> if a < b then a else r (a - b) b

-- End Exercises

addDur :: Dur -> [Dur -> Music a] -> Music a
addDur d ns = line [n d | n <- ns]

-- Exercise 5.4
apPairs :: [AbsPitch] -> [AbsPitch] -> [(AbsPitch, AbsPitch)]
apPairs aps1 aps2 = [(ap1, ap2) | ap1 <- aps1, ap2 <- aps2, gt2lt8 $ abs (ap1 - ap2)]
  where
    gt2lt8 n = n > 2 && n < 8

apPairsToMusic :: [(AbsPitch, AbsPitch)] -> Music Pitch
apPairsToMusic aps = line [note hn (pitch p1) :=: note hn (pitch p2) | (p1, p2) <- aps]
  where
    absPitchToNote ap = note (durFromAbsPitch ap) $ pitch ap
    durFromAbsPitch ap = if even 2 then en else sn

-- Exercise 5.6
addDur' :: Dur -> [Dur -> Music a] -> Music a
addDur' d = line . map ($ d)

-- Exercise 5.7
-- `map (\x -> (x + 1) / 2) xs` is the same as `map ((/2) . (+1)) xs`

-- Exercise 5.8
-- `map f (map g xs)` is the same as `map (f . g) xs`
-- `map (\x -> (x + 1) / 2) xs` is the same as `map (/2) . map (+1) xs`

-- Exercise 5.10
-- f1 (f2 (*) [1,2,3,4]) 5 => [5,10,15,20]
-- One possible solution is: `(\fs x -> map ($x) fs) (map (*) [1,2,3,4]) 5` i.e. `f1 = \fs x -> map ($x) fs` and `f2 = map`