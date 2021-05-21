module Supply where

import Stream

newtype Supply s a = Supply { runSupply :: Stream s -> (Stream s, a) }

get :: Supply s s
get = Supply (\(Cons x xs) -> (xs, x))

instance Functor (Supply s) where
  fmap f (Supply supply) = Supply (\stream -> fmap f (supply stream))

instance Applicative (Supply s) where
  pure x = Supply (\s -> (s, x))
  Supply supplyF <*> Supply supplyX = Supply $ \stream ->
    let
      (stream', f) = supplyF stream
      (stream'', x) = supplyX stream'
    in
      (stream'', f x)

instance Monad (Supply s) where
  return = pure
  Supply supplyA >>= f = Supply $ \stream ->
    let
      (stream', a) = supplyA stream
    in
      runSupply (f a) stream'

evalSupply :: Supply s a -> Stream s -> a
evalSupply supply = snd . runSupply supply

data Tree a
  = Node (Tree a) (Tree a)
  | Leaf a
  deriving (Show, Eq)

labelTree :: Tree a -> Tree Integer
labelTree tree = evalSupply (go tree) nats
  where
    go :: Tree a -> Supply s (Tree s)
    go (Leaf _) = fmap Leaf get
    go (Node l r) = Node <$> go l <*> go r
