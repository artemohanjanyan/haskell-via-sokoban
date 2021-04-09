module List where

data List a = Empty | Entry a (List a)
  deriving (Eq, Show)

instance Semigroup (List a) where
  (<>) = appendList

instance Monoid (List a) where
  mempty = Empty

mapList :: (a -> b) -> List a -> List b
mapList _ Empty = Empty
mapList f (Entry c cs) = Entry (f c) (mapList f cs)

combine :: Monoid a => List a -> a
combine Empty = mempty
combine (Entry p ps) = p <> combine ps

appendList :: List a -> List a -> List a
appendList Empty ys = ys
appendList (Entry x xs) ys = x `Entry` appendList xs ys

containsList :: Eq a => a -> List a -> Bool
containsList _ Empty = False
containsList x (Entry y ys) = x == y || containsList x ys

allList :: List Bool -> Bool
allList = not . containsList False
