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
