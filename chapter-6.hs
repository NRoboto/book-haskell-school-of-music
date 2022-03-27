import Euterpea

offset' :: Dur -> Music a -> Music a
offset' d m = rest d :+: m

lineToList' :: Music a -> [Music a]
lineToList' (Prim (Rest 0)) = []
lineToList' (n :+: ns) = n : lineToList' ns
lineToList' _ = error "lineToList': argument not created by line function"

invert' :: Music Pitch -> Music Pitch
invert' m = line $ map inv l
  where
    l@(Prim (Note _ r) : _) = lineToList m
    inv (Prim (Note d p)) = note d $ pitch $ 2 * absPitch r - absPitch p
    inv (Prim (Rest d)) = rest d

-- Exercise 6.2
isNote :: Music a -> Bool
isNote (Prim (Note _ _)) = True
isNote (Prim (Rest _)) = False

musicToPitchClass :: Music (a, b) -> a
musicToPitchClass (Prim (Note _ (pc, _))) = pc

-- | Determines whether a line of music has exactly twelve notes, and each pitch class is unique
properRow :: Music Pitch -> Bool
properRow m = length (uniquePitchClasses $ map musicToPitchClass $ filter isNote $ lineToList m) == 12
  where
    uniquePitchClasses :: [PitchClass] -> [PitchClass]
    uniquePitchClasses xs = foldr (\pc pcs -> if pc `elem` pcs then pcs else pc : pcs) [] xs

-- Exercise 6.3

-- | Determines whether a line of music is a palindrome
palin :: Music Pitch -> Bool
palin = isPalindrome . map musicToPitchClass . filter isNote . lineToList
  where
    isPalindrome [] = True
    isPalindrome [_] = True
    isPalindrome l@(x : xs) = (x == last l) && isPalindrome (init xs)

-- End Exercise

trill :: Int -> Dur -> Music Pitch -> Music Pitch
trill i sDur (Prim (Note tDur p)) = if sDur >= tDur then note tDur p else note (tDur - sDur) p :+: trill (negate i) sDur (note (tDur - sDur) $ trans i p)
trill i d (Modify (Tempo r) m) = tempo r $ trill i (d * r) m
trill i d (Modify c m) = Modify c (trill i d m)
trill _ _ _ = error "trill: Input must be a single note"

trilln :: Int -> Int -> Music Pitch -> Music Pitch
trilln i nTimes m = trill i (dur m / fromIntegral nTimes) m

roll :: Dur -> Music Pitch -> Music Pitch
roll = trill 0

funkGrove :: Music Pitch
funkGrove =
  let p1 = perc LowTom qn
      p2 = perc AcousticSnare en
   in tempo 3 $ cut 8 $ forever ((p1 :+: qnr :+: p2 :+: qnr :+: p2 :+: p1 :+: p1 :+: qnr :+: p2 :+: enr) :=: roll en (perc ClosedHiHat 2))

-- Exercise 6.7
allPercusionSounds :: Dur -> Music Pitch
allPercusionSounds d = line $ map ((`perc` d) . toEnum) [0 .. 46]

-- End exercise
