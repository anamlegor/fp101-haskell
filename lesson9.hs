import Data.List
import Data.Char
import Unsafe.Coerce

data Nat = Zero
         | Succ Nat
         deriving Show

natToInteger1 Zero     = 0
natToInteger1 (Succ n) = natToInteger1 n + 1

natToInteger2 (Succ n) = natToInteger2 n + 1
natToInteger2 Zero     = 0

natToInteger3 (Succ n) = 1 + natToInteger2 n
natToInteger3 Zero     = 0

natToInteger4 = head . m
  where m Zero     = [0]
        m (Succ n) = [sum [x | x <- (1 : m n)]]

-- natToInteger5 = \ n -> genericLength [c | c <- show n, c == 'S']

integerToNat1 0 = Zero
integerToNat1 (n+1) = Succ (integerToNat1 n)

integerToNat2 (n+1) = Succ (integerToNat2 n)
integerToNat2 0 = Zero

integerToNat3 (n+1) = let m = integerToNat3 n in Succ m
integerToNat3 0 = Zero

add1 Zero n     = n
add1 (Succ m) n = Succ (add1 n m)

add2 (Succ m) n = Succ (add2 n m)
add2 Zero n     = n

add3 n Zero     = n
add3 n (Succ m) = Succ (add3 m n)

mult m Zero     = Zero
mult m (Succ n) = add1 m (mult m n)

data Tree = Leaf Integer
           | Node Tree Tree

-- occurs1 m (Leaf n) = m == n
-- occurs1 m (Node l n r)
--       | m == n = True
--       | m < n  = occurs1 m l
--       | otherwise = occurs1 m r

halve xs = splitAt (length xs `div` 2) xs
balance [x] = Leaf x
balance xs = Node (balance ys) (balance zs)
  where (ys, zs) = halve xs

instance Show Tree where
  show (Leaf x)   = "Leaf(" ++ show x ++ ")"
  show (Node x y) = "Node(" ++ show x ++ "," ++ show y ++ ")"

class Monoid a where
  mempty :: a
  (<>)   :: a -> a -> a

instance Monoid [a] where
  mempty = []
  (<>)   = (++)

class Functor1 f where
  fmap1 :: (a -> b) -> f a -> f b

instance Functor1 Maybe where
  fmap1 _ Nothing  = Nothing
  fmap1 f (Just a) = Just (f a)

class (Functor1 f) => Foldable1 f where
  fold1 :: (Monoid m) => f m -> m 

instance Foldable1 [] where
  fold1 = foldr (<>) mempty

 

