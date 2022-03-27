import Control.Monad (ap, liftM)

combineMonads :: Monad m => m a -> m b -> m (a, b)
combineMonads xs ys = do
  x <- xs
  y <- ys
  return (x, y)

-- Exercise 16.4
data Id a = Id a

instance Monad Id where
  (>>=) (Id x) fn = fn x
  return = Id

instance Applicative Id where
  pure = return
  (<*>) = ap

instance Functor Id where
  fmap = liftM

-- End exercise

data Tree a = Leaf a | Branch (Tree a) (Tree a)
  deriving (Show)

newtype Label a = Label (Int -> (Int, a))

instance Functor Label where
  fmap = liftM

instance Applicative Label where
  pure = return
  (<*>) = ap

instance Monad Label where
  return a = Label $ \s -> (s, a)
  Label lt0 >>= flt1 = Label $ \s0 ->
    let (s1, a1) = lt0 s0
        Label lt1 = flt1 a1
     in lt1 s1