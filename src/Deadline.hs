module Deadline where

import System.Random
import Graphics.Gloss.Geometry.Line
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Pure.Simulate
import Graphics.Gloss.Juicy
import Types
import Narek
import John
import Anny

-- | Запустить игру «Deadline».
runDeadline :: Images -> IO ()
runDeadline images = do
  g <- newStdGen
  play display bgColor fps (initUniverse g) (drawUniverse images) handleUniverse updateUniverse
  where
    display = InWindow "DEADLINE" (screenWidth, screenHeight) (200, 200)
    bgColor = white   -- цвет фона
    fps     = 60      -- кол-во кадров в секунду

-- | Загрузить изображения из файлов.
loadImages :: IO Images
loadImages = do
  Just pers   <- loadJuicyPNG "src/pers.png"
  Just bgrd   <- loadJuicyPNG "src/background.png"
  return Images
    { imagePers   = scale 3 3 pers
    , imageBackground = scale 3 3 bgrd 
    }

-- =========================================
-- Модель игровой вселенной
-- =========================================

-- | Инициализировать игровую вселенную, используя генератор случайных значений.
initUniverse :: StdGen -> Universe
initUniverse g = Universe
  { universePlatforms  = initPlatforms g
  , universePlayer = initPlayer
  , universeScore  = 0
  , universeBorders = 0
  , universeBackground = 0
  }

-- | Начальное состояние игрока.
initPlayer :: Player
initPlayer = Player
  { playerHeight = 300
  , playerWidth = 0
  , playerSpeed = 0
  , playerFallingSpeed  = 0
  }

-- | Инициализировать одни ворота.
initPlatform :: Width -> Platform
initPlatform h = (h, defaultOffset)

-- | Инициализировать случайный бесконечный
-- список ворот для игровой вселенной.
initPlatforms :: StdGen -> [Platform]
initPlatforms g = map initPlatform
  (randomRs platformWidthRange g)

-- =========================================
-- Отрисовка игровой вселенной
-- =========================================

-- | Отобразить игровую вселенную.
drawUniverse :: Images -> Universe -> Picture
drawUniverse images u = pictures
  [ drawBackground (imageBackground images)
  , drawPlatforms  (universePlatforms u)
  , pictures (map (drawPlayer (imagePers images)) [ (universePlayer u) ] ) 
  , drawBorders
  , drawScore  (universeScore u)
  ]

-- | Нарисовать счёт в левом верхнем углу экрана.
drawScore :: Float -> Picture
drawScore score = translate (-w) h (scale 30 30 (pictures
  [ color red (polygon [ (0, 0), (0, -2), (3, -2), (3, 0) ])            -- красный квадрат
  , translate 1 (-1.5) (scale 0.01 0.01 (color black (text (show  (truncate score)))))  -- черный счёт
  ]))
  where
    w = fromIntegral screenWidth  / 2
    h = fromIntegral screenHeight / 2

-- | Обновить состояние игровой вселенной.
updateUniverse :: Float -> Universe -> Universe
updateUniverse dt u
  | isGameOver u = resetUniverse u 
  | isOnPlatform u = upUniverse dt u (playerHeight player + dt * speed) (max (min w (playerWidth player + dt * playerSpeed player)) wm) 0 (playerSpeed player)
  | isNearPlatform u = upUniverse dt u (playerHeight player + dt * playerFallingSpeed player) (max (min w (playerWidth player)) wm) (playerFallingSpeed player + dt * gravity) 0
  | otherwise = upUniverse dt u (playerHeight player + dt * playerFallingSpeed player) (max (min w (playerWidth player + dt * playerSpeed player)) wm) (playerFallingSpeed player + dt * gravity) (playerSpeed player)
  where
    player = (universePlayer u)
    w = 200
    wm = -200 
 
upUniverse:: Float -> Universe -> Height -> Width -> Float -> Float -> Universe 
upUniverse dt u h w fs s  = u { universePlatforms  = updatePlatforms  dt (universePlatforms  u)
      , universePlayer = updatePlayer h w fs s (universePlayer u)
      , universeScore  = (universeScore u) +dt
      }

-- | Сбросить игру (начать с начала со случайными воротами).
resetUniverse :: Universe -> Universe
resetUniverse u = u
  { universePlatforms  = tail (universePlatforms u)
  , universePlayer = initPlayer
  , universeScore  = 0
  }

isOnPlatform :: Universe -> Bool
isOnPlatform u = playerBelowFloor || playerHitsPlatform
  where
    playerHitsPlatform   = collision (universePlayer u) (universePlatforms u)
    playerBelowFloor = playerHeight (universePlayer u) < - fromIntegral screenHeight

-- |
isNearPlatform :: Universe -> Bool
isNearPlatform u = playerHitsPlatform
  where
    playerHitsPlatform = collisionNear (universePlayer u) (universePlatforms u)
  
-- | Конец игры?
isGameOver :: Universe -> Bool
isGameOver u = playerBelowFloor || playerBelowRoof
  where
    playerBelowRoof = playerHeight (universePlayer u) >  screenUp - 30
    playerBelowFloor = playerHeight (universePlayer u) < screenDown + 30

-- | Сталкивается ли игрок с любыми из
-- бесконечного списка ворот?
collision :: Player -> [Platform] -> Bool
collision _ [] = False
collision player platforms = or (map (collides player) (takeWhile onScreen (absolutePlatforms platforms)))
  where
    onScreen (_, offset) = offset - platformHeight > screenDown

-- | Сталкивается ли игрок с любыми из
-- бесконечного списка платформ?
collisionNear :: Player -> [Platform] -> Bool
collisionNear _ [] = False
collisionNear player platforms = or (map (collidesNear player) (takeWhile onScreen (absolutePlatforms platforms)))
  where
    onScreen (_, offset) = offset - platformHeight > screenDown

-- | Сталкивается ли игрок с платформами?
collidesNear :: Player -> Platform -> Bool
collidesNear player platform = or
  [ polygonBoxCollides polygon box
  | polygon <- playerPolygons player
  , box     <- platformBoxes platform ]

-- |  Становится ли игрок на платформу?
collides :: Player -> Platform -> Bool
collides player platform = and [((playerHeight player - 1200 * 0.03)< (snd platform + platformHeight)), ((playerHeight player - 1200 * 0.03)> (snd platform)), ((playerWidth player - 800*0.03) < (fst platform + platformWidth /2)), ((playerWidth player + 800*0.03) > (fst platform - platformWidth /2))]


-- | Упрощённая проверка на пересечение многоугольников.
polygonBoxCollides :: Path -> (Point, Point) -> Bool
polygonBoxCollides xs (lb, rt) = or
  [ not (segClearsBox p1 p2 lb rt)
  | (p1, p2) <- zip xs (tail (cycle xs))  ]

absoluteValue :: Float -> Float
absoluteValue n | n >= 0 = n
                | otherwise  = -n
-- | Обновить состояние игрока.
-- Игрок не может прыгнуть выше потолка.
 -- | Обновить состояние игрока.

updatePlayer :: Height -> Width -> Float -> Float -> Player -> Player
updatePlayer h w fs s player = player
  { playerHeight = h
  , playerWidth = w
  , playerFallingSpeed  = fs
  , playerSpeed = s
  }
  
-- =========================================
-- Константы, параметры игры
-- =========================================
