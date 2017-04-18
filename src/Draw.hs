module Draw where

import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Interface.Pure.Game
import Types

-- | Отобразить все платформы игровой вселенной, вмещающиеся в экран.
drawPlatforms :: [Platform] -> Picture
drawPlatforms = pictures . map drawPlatform . takeWhile onScreen
  where
    onScreen (_, offset,_) = offset - platformHeight > screenDown

-- | Нарисовать одну платформу.
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

-- | Нарисовать игрока.
drawPlayer :: Picture -> Player -> Picture
drawPlayer image player = translate x y (scale 0.1075 0.1075 image)
  where
    (x, y) = (playerWidth player, playerHeight player)

-- | Нарисовать задний фон.
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

-- | Нарисовать конец игры.
drawGameOver :: Picture -> Maybe Point -> Picture
drawGameOver _ Nothing = blank
drawGameOver image (Just (x, y)) = (scale x y image)

-- | Нарисовать счёт в левом верхнем углу экрана.
drawScore :: Float -> Picture
drawScore score = translate (-w) h (scale 30 30 (pictures
  [ color red (polygon [ (0, 0), (0, -2), (3, -2), (3, 0) ])            -- красный квадрат
  , translate 1 (-1.5) (scale 0.01 0.01 (color black (text (show  (truncate score)))))  -- черный счёт
  ]))
  where
    w = fromIntegral screenWidth  / 2
    h = fromIntegral screenHeight / 2

-- | Отобразить игровую вселенную.
drawUniverse :: Images -> Universe -> Picture
drawUniverse images u = pictures
  [ drawBackground (imageBackground images)
  , drawPlatforms  (universePlatforms u)
  , pictures (map (drawPlayer (imagePers images)) [ (universePlayer u) ] ) 
  , drawBorders
  , drawScore  (universeScore u)
  , drawGameOver (imageGameOver images) (universeGameOver u)
  ]
