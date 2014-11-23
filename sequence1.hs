sequence' [] = return []
sequence' (m : ms)
  = m >>=
      \ a ->
        do as <- sequence' ms
           return (a : as)

sequence'' ms = foldr func (return []) ms
  where
        func m acc
          = do x <- m
               xs <- acc
               return (x : xs)

sequence''' [] = return []
sequence''' (m : ms)
  = do a <- m
       as <- sequence' ms
       return (a : as)

