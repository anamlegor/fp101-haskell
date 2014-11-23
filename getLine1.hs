getLine' = get []
get xs
  = do x <- getChar
       case x of
           '\n' -> return xs
           _ -> get (xs ++ [x])

