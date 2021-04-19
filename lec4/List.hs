module List where

data List a = Empty | Entry a (List a)
  deriving (Eq, Show)

infixr `Entry`

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

listLength :: List a -> Integer
listLength Empty = 0
listLength (Entry _ xs) = 1 + listLength xs

filterList :: (a -> Bool) -> List a -> List a
filterList _ Empty = Empty
filterList p (Entry x xs) = (if p x then Entry x else id) (filterList p xs)

nth :: List a -> Integer -> a
nth _ n | n < 1 = error "list index < 1"
nth (Entry x _) 1 = x
nth (Entry _ xs) n = nth xs (n - 1)
nth Empty _ = error "list is too short"

isGraphClosed :: Eq a => a -> (a -> List a) -> (a -> Bool) -> Bool
isGraphClosed initial adjacent isOk = go Empty (Entry initial Empty)
  where
    go _ Empty = True
    go seen (Entry x xs)
      | containsList x seen = go seen xs
      | otherwise = isOk x && go (Entry x seen) (adjacent x `appendList` xs)
