foldl' f a [] = a
foldl' f a (x : xs) = foldl' f (f a x) xs

foldLeftM' f a [] = return a
foldLeftM' f a (x : xs)
  = do b <- f a x
       foldLeftM' f b xs


