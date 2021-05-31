module MonadToApplicative where


func0 :: Monad f => (a -> a) -> f a -> f a
func0 f xs = do
    x <- xs
    return (f (f x))

func0' :: Functor f => (a -> a) -> f a -> f a
func0' f xs = (f . f) <$> xs


func1 :: Monad f => f a -> f (a,a)
func1 xs = xs >>= (\x -> return (x,x))

func1' :: Functor f => f a -> f (a, a)
func1' xs = fmap (\x -> (x, x)) xs


func2 :: Monad f => f a -> f (a,a)
func2 xs = xs >>= (\x -> xs >>= \y -> return (x,y))

func2' :: Applicative f => f a -> f (a, a)
func2' xs = (,) <$> xs <*> xs


func3 :: Monad f => f a -> f (a,a)
func3 xs = xs >>= (\x -> xs >>= \_ -> return (x,x))

func3' :: Applicative f => f a -> f (a, a)
func3' xs = (\x _ -> (x, x)) <$> xs <*> xs


func4 :: Monad f => f a -> f a -> f (a,a)
func4 xs ys = xs >>= (\x -> ys >>= \y -> return (x,y))

func4' :: Applicative f => f a -> f a -> f (a,a)
func4' xs ys = (,) <$> xs <*> ys


func5 :: Monad f => f Integer -> f Integer -> f Integer
func5 xs ys = do
    x <- xs
    let x' = x + 1
    y <- (+1) <$> ys
    return (x' + y)

func5' :: Applicative f => f Integer -> f Integer -> f Integer
func5' xs ys = (\x y -> x + y + 2) <$> xs <*> ys


func6 :: Monad f => f Integer -> f (Integer,Integer)
func6 xs = do
    x <- xs
    return $ if x > 0 then (x, 0)
                      else (0, x)

func6' :: Functor f => f Integer -> f (Integer,Integer)
func6' = fmap $ \x ->
  if x > 0 then (x, 0) else (0, x)


func7 :: Monad f => f Integer -> f (Integer,Integer)
func7 xs = do
    x <- xs
    if x > 0 then return (x, 0)
             else return (0, x)

func7' :: Functor f => f Integer -> f (Integer,Integer)
func7' = fmap $ \x ->
  if x > 0 then (x, 0) else (0, x)


func8 :: Monad f => f Integer -> Integer -> f Integer
func8 xs x = pure (+) <*> xs <*> pure x

func8' :: Functor f => f Integer -> Integer -> f Integer
func8' xs x = (+x) <$> xs


func9 :: Monad f => f Integer -> f Integer -> f Integer -> f Integer
func9 xs ys zs = xs >>= \x -> if even x then ys else zs

-- func9' can't do because effect depends on the value of computation xs


func10 :: Monad f => f Integer -> f Integer
func10 xs = do
    x <- xs >>= (\x -> return (x * x))
    return (x + 10)

func10' :: Functor f => f Integer -> f Integer
func10' = fmap (\x -> x * x + 10)
