module Deadline where

import System.Random
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Line
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Pure.Simulate
import Graphics.Gloss.Juicy
import Types
import Narek
import John
import Anny
import Kate

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
  , isOnPlatformNow = False
  , playerSpeed = 0
  , playerFallingSpeed  = 0
  }

-- | Инициализировать одну платформу
initPlatform :: Width -> Platform
initPlatform h = (h, defaultOffset)

-- | Инициализировать случайный бесконечный
-- список платформ для игровой вселенной
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

-- | Нарисовать счёт в левом верхнем углу экрана
drawScore :: Score -> Picture
drawScore score = translate (-w) h (scale 30 30 (pictures
  [ color red (polygon [ (0, 0), (0, -2), (3, -2), (3, 0) ])            -- красный квадрат
  , translate 1 (-1.5) (scale 0.01 0.01 (color black (text (show score))))  -- черный счёт
  ]))
  where
    w = fromIntegral screenWidth  / 2
    h = fromIntegral screenHeight / 2


-- | Обновить состояние игровой вселенной.
updateUniverse :: Float -> Universe -> Universe
updateUniverse dt u
  | isOnPlatform u = u { universePlatforms  = updatePlatforms  dt (universePlatforms  u)
      , universePlayer = updatePlayer True dt (universePlayer u)
      , universeScore  = universeScore u + scorePlatforms (universePlatforms u)
      }
  | otherwise = u
      { universePlatforms  = updatePlatforms  dt (universePlatforms  u)
      , universePlayer = updatePlayer False dt (universePlayer u)
      , universeScore  = universeScore u + scorePlatforms (universePlatforms u)
      }
  where
    scorePlatforms = length . takeWhile isPast . dropWhile wasPast . absolutePlatforms
    -- платформы окажутся внизу игрока в этом кадре?
    isPast (offset, _) = offset + platformWidth / 2 - dt * speed < 0
    -- ворота уже были внизу игрока в предыдущем кадре?
    wasPast (offset, _) = offset + platformWidth / 2 < 0




-- | Сбросить игру (начать с начала со случайными платформами).
resetUniverse :: Universe -> Universe
resetUniverse u = u
  { universePlatforms  = tail (universePlatforms u)
  , universePlayer = initPlayer
  , universeScore  = 0
  }

-- | Конец игры?
isOnPlatform :: Universe -> Bool
isOnPlatform u = playerBelowFloor || playerHitsPlatform
  where
    playerHitsPlatform   = collision (universePlayer u) (universePlatforms u)
    playerBelowFloor = playerHeight (universePlayer u) < - fromIntegral screenHeight

-- | Конец игры?
wasOnPlatform :: Player -> Bool
wasOnPlatform player = (isOnPlatformNow player)
  
-- | Конец игры?
isGameOver :: Universe -> Bool
isGameOver u = playerBelowFloor || playerHitsPlatform
  where
    playerHitsPlatform   = collision (universePlayer u) (universePlatforms u)
    playerBelowFloor = playerHeight (universePlayer u) < - fromIntegral screenHeight

-- | Сталкивается ли игрок с любыми из
-- бесконечного списка платформ?
collision :: Player -> [Platform] -> Bool
collision _ [] = False
collision player platforms = or (map (collides player) (takeWhile onScreen (absolutePlatforms platforms)))
  where
    onScreen (_, offset) = offset - platformHeight< screenUp

-- | Сталкивается ли игрок с платформами?
collides :: Player -> Platform -> Bool
collides player platform = or
  [ polygonBoxCollides polygon box
  | polygon <- playerPolygons player
  , box     <- platformBoxes platform ]

-- | Упрощённая проверка на пересечение многоугольников
polygonBoxCollides :: Path -> (Point, Point) -> Bool
polygonBoxCollides xs (lb, rt) = or
  [ not (segClearsBox p1 p2 lb rt)
  | (p1, p2) <- zip xs (tail (cycle xs)) ]

absoluteValue :: Float -> Float
absoluteValue n | n >= 0 = n
                | otherwise  = -n


-- | Обновить состояние игрока.
updatePlayer :: Bool -> Float -> Player -> Player
updatePlayer True dt player 
  | wasOnPlatform player = player
  { playerHeight = playerHeight player + dt * speed
  , playerWidth = max (min w (playerWidth player + dt * playerSpeed player)) wm
  , playerFallingSpeed  = 0
  , playerSpeed = playerSpeed player
  , isOnPlatformNow = True
  }
  | otherwise = player
  { playerHeight = playerHeight player + dt * speed
  , playerWidth = max (min w (playerWidth player + dt * playerSpeed player)) wm
  , playerFallingSpeed  = 0
  , playerSpeed = 0
  , isOnPlatformNow = True
  }
   where
    h = fromIntegral screenHeight / 2
    w = 200
    wm = -200

updatePlayer False dt player = player
  { playerHeight = playerHeight player + dt * playerFallingSpeed player
  , playerWidth = max (min w (playerWidth player + dt * playerSpeed player)) wm
  , playerFallingSpeed  = playerFallingSpeed player + dt * gravity
  , playerSpeed = playerSpeed player
  , isOnPlatformNow = False
  }
  where
    h = fromIntegral screenHeight / 2
    w = 200
    wm = -200
-- =========================================
-- Константы, параметры игры
-- =========================================