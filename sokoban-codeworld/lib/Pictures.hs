{-# LANGUAGE OverloadedStrings #-}

module Pictures
  ( wall
  , ground
  , storage
  , box

  , player

  , startScreen
  ) where

import CodeWorld

import Core

rgb :: Double -> Double -> Double -> Color
rgb r g b = RGB (r / 256) (g / 256) (b / 256)

wall, ground, storage, box :: Picture
wall = horizontal & vertical & background
  where
    horizontal =
      polyline [(0.5, 0.5), (-0.5, 0.5)] &
      polyline [(0.5, 0.25), (-0.5, 0.25)] &
      polyline [(0.5, 0), (-0.5, 0)] &
      polyline [(0.5, -0.25), (-0.5, -0.25)] &
      polyline [(0.5, -0.5), (-0.5, -0.5)]
    vertical =
      polyline [(-0.5, 0.5), (-0.5, 0.25)] &
      polyline [(-0.5, 0), (-0.5, -0.25)] &
      polyline [(-0.25, 0.25), (-0.25, 0)] &
      polyline [(-0.25, -0.25), (-0.25, -0.5)] &
      polyline [(0, 0.5), (0, 0.25)] &
      polyline [(0, 0), (0, -0.25)] &
      polyline [(0.25, 0.25), (0.25, 0)] &
      polyline [(0.25, -0.25), (0.25, -0.5)]
    background =
      colored (rgb 161 149 85) (solidRectangle 1 1)
ground = colored (rgb 222 214 174) (solidRectangle 1 1)
storage = colored (rgb 215 149 133) (solidCircle 0.125) & ground
box =
  square 0.5 &
  square 0.35 &
  polygon diagonal &
  colored (dark color) (solidPolygon diagonal) &
  colored (darker 0.25 color) (solidPolygon
    [ (-0.35, -0.35)
    , (-0.28, -0.28)
    , (-0.28, 0.28)
    , (0.28, 0.28)
    , (0.35, 0.35)
    , (-0.35, 0.35)
    ]) &
  colored (light color) (solidPolygon
    [ (-0.35, -0.35)
    , (-0.28, -0.42)
    , (0.42, -0.42)
    , (0.42, 0.28)
    , (0.35, 0.35)
    , (0.35, -0.35)
    ]) &
  colored (light color) (solidPolygon
    [ (-0.43, -0.43)
    , (-0.5, -0.5)
    , (-0.5, 0.5)
    , (0.5, 0.5)
    , (0.43, 0.43)
    , (-0.43, 0.43)
    ]) &
  colored color (solidRectangle 1 1)
  where
    color = rgb 241 174 66
    square r = polygon [(-r, -r), (-r, r), (r, r), (r, -r)]
    diagonal =
      [ (-0.35, -0.35)
      , (-0.35, -0.25)
      , (0.25, 0.35)
      , (0.35, 0.35)
      , (0.35, 0.25)
      , (-0.25, -0.35)
      ]

player :: Direction -> Picture
player R = translated 0 0.3 cranium
         & polyline [(0,0),(0.3,0.05)]
         & polyline [(0,0),(0.3,-0.05)]
         & polyline [(0,-0.2),(0,0.1)]
         & polyline [(0,-0.2),(0.1,-0.5)]
         & polyline [(0,-0.2),(-0.1,-0.5)]
  where cranium = circle 0.18
                & sector (7/6*pi) (1/6*pi) 0.18
player L = scaled (-1) 1 (player R) -- Cunning!
player U = translated 0 0.3 cranium
         & polyline [(0,0),(0.3,0.05)]
         & polyline [(0,0),(-0.3,0.05)]
         & polyline [(0,-0.2),(0,0.1)]
         & polyline [(0,-0.2),(0.1,-0.5)]
         & polyline [(0,-0.2),(-0.1,-0.5)]
  where cranium = solidCircle 0.18
player D = translated 0 0.3 cranium
         & polyline [(0,0),(0.3,-0.05)]
         & polyline [(0,0),(-0.3,-0.05)]
         & polyline [(0,-0.2),(0,0.1)]
         & polyline [(0,-0.2),(0.1,-0.5)]
         & polyline [(0,-0.2),(-0.1,-0.5)]
  where cranium = circle 0.18
                & translated   0.06  0.08 (solidCircle 0.04)
                & translated (-0.06) 0.08 (solidCircle 0.04)

startScreen :: Picture
startScreen = scaled 3 3 (lettering "Sokoban!")
