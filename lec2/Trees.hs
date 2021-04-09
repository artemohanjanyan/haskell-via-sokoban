module Main where

import CodeWorld

tree :: Integer -> Picture -> Picture
tree 0 b = b
tree n b =
  translated 0 1 (rotated (pi/10) (tree (n-1) b) & rotated (- pi/10) (tree (n-1) b)) &
  polyline [(0,0),(0,1)]

blossom :: Double -> Picture
blossom r = colored yellow $ solidCircle ((min 10 r) / 70)

main :: IO ()
main = animationOf (\t -> tree 8 (blossom t))
