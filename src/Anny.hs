module Anny where

import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Line
import Graphics.Gloss.Interface.Pure.Game
import Types

-- | Рассчитать абсолютное положение ворот.
absolutePlatforms :: [Platform] -> [Platform]
absolutePlatforms = go (-800)
  where
    go _ [] = []
    go s ((w, o) : gs) = (w, o + s) : go (s + o) gs

-- | Отобразить все ворота игровой вселенной, вмещающиеся в экран.
drawPlatforms :: [Platform] -> Picture
drawPlatforms = pictures . map drawPlatform . takeWhile onScreen . absolutePlatforms
  where
    onScreen (_, offset) = offset - platformHeight < screenUp

-- | Нарисовать одни ворота.
drawPlatform :: Platform -> Picture
drawPlatform = color black . pictures . map drawBox . platformBoxes
  where
    drawBox ((l, b), (r, t)) = polygon
      [ (l, b), (r, b), (r, t), (l, t) ]

-- | Обновить ворота игровой вселенной.
updatePlatforms :: Float -> [Platform] -> [Platform]
updatePlatforms _ [] = []
updatePlatforms dt ((width, offset) : platforms)
  | dy > pos  = updatePlatforms dt' platforms
  | otherwise = (width, offset + dy) : platforms
  where
    pos = -offset*10 + screenUp + platformHeight
    dy  =   dt * speed
    dt' = dt - (offset ) / speed