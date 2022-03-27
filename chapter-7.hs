import Euterpea (Dur, Music, Primitive (..), cut, dur, remove)

-- Exercise 7.2
data Color = Red | Green | Blue

instance Eq Color where
  Red == Red = True
  Green == Green = True
  Blue == Blue = True
  _ == _ = False

instance Ord Color where
  Red <= Green = True
  Red <= Blue = True
  Green <= Blue = True
  x <= y = x == y

instance Show Color where
  show Red = "Red"
  show Green = "Green"
  show Blue = "Blue"

instance Enum Color where
  toEnum 0 = Red
  toEnum 1 = Green
  toEnum 2 = Blue
  fromEnum Red = 0
  fromEnum Green = 1
  fromEnum Blue = 2

instance Bounded Color where
  minBound = Red
  maxBound = Blue

-- Exercise 7.3
class Temporal a where
  durT :: a -> Dur
  cutT :: Dur -> a -> a
  removeT :: Dur -> a -> a

instance Temporal (Music a) where
  durT = dur
  cutT = cut
  removeT = remove

instance Temporal (Primitive a) where
  durT (Note d _) = d
  durT (Rest d) = d

  cutT d (Note d' pc) = Note (max 0 $ d' - d) pc
  cutT d (Rest d') = Rest (max 0 $ d' - d)

  removeT = cutT

-- Exercise 7.4
instance (Enum a, Bounded a, Eq b) => Eq (a -> b) where
  f == g = all (\x -> f x == g x) [minBound .. maxBound]

-- End exercise