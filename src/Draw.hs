module Draw where

import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Interface.Pure.Game
import Types

-- | Создать бесконечный список платформ.
absolutePlatforms :: [Platform] -> [Platform]
absolutePlatforms = go 0  
  where
    go  _ [] = []
    go  s ((w, o) : gs) = (w, s - o) : (go (s - o) gs)

-- | Отобразить все платформы игровой вселенной, вмещающиеся в экран.
drawPlatforms :: [Platform] -> Picture
drawPlatforms = pictures . map drawPlatform . takeWhile onScreen . absolutePlatforms
  where
    onScreen (_, offset) = offset - platformHeight > screenDown

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
  [ color red (polygon [ (0, 0), (0, -2), (15, -2), (15, 0) ])            -- верхняя граница
  , color red (polygon [ (0, -21.5), (0, -24), (15, -24), (15, -21.5) ]) -- нижняя граница
  , color red (polygon [ (0, 0), (0, -2), (3, -2), (3, 0) ])
  ]))
  where
    w = fromIntegral screenWidth / 2
    h = fromIntegral screenHeight / 2

-- | Нарисрвать текст
drawText :: Float -> Picture
drawText score = translate (-w) h (scale 30 30 (pictures 
  (concat [(drawTextList 5 4.5 (-1.5) "DEADLINE")
          ,(drawTextList 3 4.35 (-22.9) "exhaustion")
          ,(drawTextList 3 1 (-1.5) (show (truncate score)))])))
  where
    w = fromIntegral screenWidth / 2
    h = fromIntegral screenHeight / 2

-- | Составить список текста со смещением
drawTextList :: Int -> Float -> Float -> String -> [Picture]
drawTextList 0 _ _ _ = []
drawTextList k w h s = (drawTextFunc w h s) : (drawTextList (k-1) (w+0.02) h s) 

-- | Отрисоать одно слово
drawTextFunc :: Float -> Float -> String -> Picture
drawTextFunc w h s = translate w (h) (scale 0.01 0.01 (color black (text s)))

-- | Нарисовать конец игры.
drawGameOver :: Picture -> Maybe Point -> Picture
drawGameOver _ Nothing = blank
drawGameOver image (Just (x, y)) = (scale x y image)

-- | Отобразить игровую вселенную.
drawUniverse :: Images -> Universe -> Picture
drawUniverse images u = pictures
  [ drawBackground (imageBackground images)
  , drawPlatforms  (universePlatforms u)
  , pictures (map (drawPlayer (imagePers images)) [ (universePlayer u) ] ) 
  , drawBorders
  , drawText (universeScore u)
  --, drawScore  (universeScore u)
  , drawGameOver (imageGameOver images) (universeGameOver u)
  ]
