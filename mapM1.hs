sequence' [] = return []
sequence' (m : ms)
  = m >>=
      \ a ->
        do as <- sequence' ms
           return (a : as)

sequence_' ms = foldr (>>) (return ()) ms

mapM' f as = sequence' (map f as)

mapM'' f [] = return []
mapM'' f (a : as)
  = f a >>= \ b -> mapM'' f as >>= \ bs -> return (b : bs)

mapM''' f [] = return []
mapM''' f (a : as)
  = do b <- f a
       bs <- mapM''' f as
       return (b : bs)

