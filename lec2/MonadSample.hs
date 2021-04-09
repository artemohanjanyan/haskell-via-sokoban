import System.Random

data Coord = Coord Int Int
  deriving Show

andThen
  :: (StdGen -> (something, StdGen))
  -> (something -> (StdGen -> (somethingelse, StdGen)))
  -> (StdGen -> (somethingelse, StdGen))
andThen a fb = \gen -> let (aRes, gen') = a gen in fb aRes gen'

andJust :: something -> (StdGen -> (something, StdGen))
andJust a = \gen -> (a, gen)

rand :: StdGen -> (Int, StdGen)
rand = randomR (-10, 10)

randomCoord :: StdGen -> (Coord, StdGen)
randomCoord =
  rand `andThen` \x ->
  rand `andThen` \y ->
  andJust (Coord x y)

main :: IO ()
main = do
  stdGen <- getStdGen
  print (fst (randomCoord stdGen))
