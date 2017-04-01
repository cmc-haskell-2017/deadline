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
drawPlatform p = (pictures [ color (makeColorI 0 66 72 255) (pictures (map drawBox  (platformBoxes p)))
                           , color (makeColorI 0 120 170 255) (pictures (map drawBox1 (platformBoxes p)))
                           , color (makeColorI 0 66 72 255) (pictures (map drawBox2  (platformBoxes p)))
                           , color (makeColorI 0 66 72 255) (pictures (map drawBox3  (platformBoxes p))) ])
  where
    drawBox ((l, b), (r, t)) = polygon
      [ (l, b), (r, b), (r, t), (l, t) ]
    drawBox1 ((l, b), (r, t)) = polygon
      [ (l+3, b+3), (r-3, b+3), (r-3, t-3), (l+3, t-3) ]
    drawBox2 ((l, b), (r, t)) = polygon
      [ (l, b+3), (l+3, b), (r, t-3), (r-3, t) ]
    drawBox3 ((l, b), (r, t)) = polygon
      [ (l, t-3), (l+3, t), (r, b+3), (r-3, b) ]

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