module Main where

import Control.Monad (replicateM)
import System.Random

import Supply
import Stream

type Rand a = Supply Integer a

randomDice :: RandomGen g => g -> Stream Integer
randomDice gen =
    let (roll, gen') = randomR (1,6) gen
    in Cons roll (randomDice gen')

runRand :: Rand a -> IO a
runRand r = do
    stdGen <- getStdGen
    let diceRolls = randomDice stdGen
    return $ evalSupply r diceRolls

averageOfTwo :: Rand Double
averageOfTwo = do
    d1 <- get
    d2 <- get
    return $ fromIntegral (d1 + d2) / 2

bestOutOfTwo :: Rand Double
bestOutOfTwo = do
    d1 <- get
    d2 <- get
    return $ fromIntegral $ if (d1 > d2) then d1 else d2

-- Look, ma, I’m recursive!
sumUntilOne :: Rand Double
sumUntilOne = do
    d <- get
    if (d == 1) then return 0
                else do s <- sumUntilOne
                        return (s + fromIntegral d)

sample :: Int -> Rand Double -> Rand (Double, Double)
sample n what = do
    samples <- replicateM n what
    return (maximum samples, sum samples / fromIntegral n)

main :: IO ()
main = mapM_ go [ ("average of two", averageOfTwo)
                , ("bestOutOfTwo",   bestOutOfTwo)
                , ("sumUntilOne",    sumUntilOne)
                ]
  where
    n = 10000
    go (name, what) = do
        (max, avg) <- runRand (sample n what)
        putStrLn $ "Playing \"" ++ name ++ "\" " ++ show n ++ " times " ++
                   "yields a max of " ++ show max ++ " and an average of " ++
                   show avg ++ "."
