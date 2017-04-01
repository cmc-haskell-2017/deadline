module Narek where

import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Line
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Pure.Simulate
import Graphics.Gloss.Juicy
import Types

-- | Многоугольники, определяющие игрока.
playerPolygons :: Player -> [Path]
playerPolygons player = map (map move)
  [ [ (-800, -1200), (800, -1200), (800, 1000), (-800, 1000) ]
  ]
  where
    move (x, y) = (playerWidth player, playerHeight player) + mulSV 0.03 (x, y)

-- | Нарисовать игрока.
drawPlayer :: Picture -> Player -> Picture
drawPlayer image player =translate x y (scale 0.1 0.1 image)
  where
    (x, y) = (playerWidth player, playerHeight player)

drawBackground :: Picture -> Picture
drawBackground image = (scale 0.12 0.12 image)


-- | Нарисовать границы сверху и снизу.
drawBorders :: Picture
drawBorders = translate (-w) h (scale 30 30 (pictures
  [ color red (polygon [ (0, 0), (0, -2), (sw, -2), (sw, 0) ])            -- верхняя граница
  , translate 4.5 (-1.5) (scale 0.01 0.01 (color black (text "DEADLINE")))  -- "deadline"
  , translate 4.6 (-1.5) (scale 0.01 0.01 (color black (text "DEADLINE")))
  , translate 4.55 (-1.5) (scale 0.01 0.01 (color black (text "DEADLINE")))
  , translate 4.52 (-1.5) (scale 0.01 0.01 (color black (text "DEADLINE")))
  , translate 4.57 (-1.5) (scale 0.01 0.01 (color black (text "DEADLINE")))
  , color red (polygon [ (0, -sh+1.5), (0, -sh-1), (sw, -sh-1), (sw, -sh+1.5) ]) -- нижняя граница
  , translate 4.35 (-sh+0.1) (scale 0.01 0.01 (color black (text "exhaustion"))) -- "exhaustion"
  , translate 4.32 (-sh+0.1) (scale 0.01 0.01 (color black (text "exhaustion")))
  , translate 4.37 (-sh+0.1) (scale 0.01 0.01 (color black (text "exhaustion")))
  ]))
  where
    sw = 15 
    sh = 23
    w = fromIntegral screenWidth / 2
    h = fromIntegral screenHeight / 2