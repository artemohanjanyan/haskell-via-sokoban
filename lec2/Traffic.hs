{-# LANGUAGE OverloadedStrings #-}

module Main where

import CodeWorld

botCircle, midCircle, topCircle :: Color -> Picture
botCircle c = colored c (translated 0 (-3) (solidCircle 1))
midCircle c = colored c (translated 0   0  (solidCircle 1))
topCircle c = colored c (translated 0   3  (solidCircle 1))

frame :: Picture
frame = rectangle 2.5 8.5

trafficLight :: Int -> Picture
trafficLight n
  | n >= 0 && n < 3 = topCircle black & midCircle black & botCircle green & frame
  | n >= 3 && n < 4 = topCircle black & midCircle yellow & botCircle black & frame
  | n >= 4 && n < 6 = topCircle red & midCircle black & botCircle black & frame
  | n >= 6 && n < 7 = topCircle red & midCircle yellow & botCircle black & frame
  | otherwise = frame

trafficController :: Double -> Picture
trafficController t = trafficLight (round t `mod` 7)

main :: IO ()
main = animationOf trafficController
