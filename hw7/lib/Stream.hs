module Stream where

data Stream a = Cons a (Stream a)

foldStream :: (a -> b -> b) -> Stream a -> b
foldStream f (Cons x xs) = f x (foldStream f xs)

streamToList :: Stream a -> [a]
streamToList = foldStream (:)

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

streamRepeat :: a -> Stream a
streamRepeat x = let xs = Cons x xs in xs

streamTake :: Int -> Stream a -> a
streamTake n _ | n < 0 = error "negative stream index"
streamTake 0 (Cons x _) = x
streamTake n (Cons _ xs) = streamTake (n - 1) xs

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f = foldStream (Cons . f)

streamIterate :: (a -> a) -> a -> Stream a
streamIterate f z = Cons z (streamIterate f (f z))

streamInterleave :: Stream a -> Stream a -> Stream a
streamInterleave (Cons x xs) ys = Cons x (streamInterleave ys xs)

nats :: Stream Integer
nats = streamIterate (+1) 0

ruler :: Stream Integer
ruler = streamInterleave (streamRepeat 0) (streamMap (+1) ruler)
