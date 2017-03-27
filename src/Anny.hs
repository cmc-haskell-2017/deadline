module Anny where

import Graphics.Gloss.Interface.Pure.Game
import Types

-- | Рассчитать абсолютное положение платформ
absolutePlatforms :: [Platform] -> [Platform]
absolutePlatforms = go 0  
  where
    go  _ [] = []
    go  s ((w, o) : gs) = (w, s - o) : (go (s - o) gs)

-- | Отобразить все платформы игровой вселенной, вмещающиеся в экран
drawPlatforms :: [Platform] -> Picture
drawPlatforms = pictures . map drawPlatform . takeWhile onScreen . absolutePlatforms
  where
    onScreen (_, offset) = offset - platformHeight > screenDown

-- | Нарисовать одну платформу
drawPlatform :: Platform -> Picture
drawPlatform = color black . pictures . map drawBox . platformBoxes
  where
    drawBox ((l, b), (r, t)) = polygon
      [ (l, b), (r, b), (r, t), (l, t) ]

-- | Обновить платформы игровой вселенной
updatePlatforms :: Float -> [Platform] -> [Platform]
updatePlatforms _ [] = []
updatePlatforms dt ((width, offset) : platforms)
  | dy > pos  = updatePlatforms dt' platforms
  | otherwise = (width, offset - dy) : platforms
  where
    pos = screenUp - offset + platformHeight
    dy  =  dt * speed
    dt' = dt -  offset / speed