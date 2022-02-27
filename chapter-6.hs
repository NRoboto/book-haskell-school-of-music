import Euterpea

offset' :: Dur -> Music a -> Music a
offset' d m = rest d :+: m

lineToList' :: Music a -> [Music a]
lineToList' (Prim (Rest 0)) = []
lineToList' (n :+: ns) = n : lineToList' ns
lineToList' _ = error "lineToList': argument not created by line function"

invert' :: Music Pitch -> Music Pitch
invert' m = line $ map inv l
            where l@(Prim (Note _ r):_) = lineToList m
                  inv (Prim (Note d p)) = note d $ pitch $ 2 * absPitch r - absPitch p
                  inv (Prim (Rest d)) = rest d

-- Exercise 6.2
isNote :: Music a -> Bool
isNote (Prim (Note _ _)) = True 
isNote (Prim (Rest _)) = False

musicToPitchClass :: Music (a, b) -> a
musicToPitchClass (Prim (Note _ (pc, _))) = pc

-- |Determines whether a line of music has exactly twelve notes, and each pitch class is unique
properRow :: Music Pitch -> Bool
properRow m = length (uniquePitchClasses $ map musicToPitchClass $ filter isNote $ lineToList m) == 12
            where uniquePitchClasses :: [PitchClass] -> [PitchClass]
                  uniquePitchClasses xs = foldr (\pc pcs -> if pc `elem` pcs then pcs else pc:pcs) [] xs

-- Exercise 6.3
-- |Determines whether a line of music is a palindrome
palin :: Music Pitch -> Bool
palin = isPalindrome . map musicToPitchClass . filter isNote . lineToList
      where isPalindrome [] = True
            isPalindrome [_] = True
            isPalindrome l@(x:xs) = (x == last l) && isPalindrome (init xs)