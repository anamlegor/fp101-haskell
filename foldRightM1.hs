foldr' f a [] = a
foldr' f a (x : xs) = f x (foldr' f a xs)

foldRightM' f a [] = return a
foldRightM' f a (x : xs)
  = do b <- foldRightM' f a xs
       c <- f x b
       return c


