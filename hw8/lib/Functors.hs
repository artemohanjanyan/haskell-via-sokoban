module Functors where

data ComplicatedA a b
    = Con1 a b
    | Con2 [Maybe (a -> b)]

data ComplicatedB f g a b
    = Con3 (f a)
    | Con4 (g b)
    | Con5 (g (g [b]))

instance Functor (ComplicatedA a) where
  fmap f (Con1 x y) = Con1 x (f y)
  fmap f (Con2 xs) = Con2 $ map (fmap (fmap f)) xs

instance Functor g => Functor (ComplicatedB f g a) where
  fmap _ (Con3 x) = Con3 x
  fmap f (Con4 x) = Con4 $ fmap f x
  fmap f (Con5 x) = Con5 $ fmap (fmap (map f)) x
