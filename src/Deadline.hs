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
  Just gover  <- loadJuicyPNG "src/gameover.png"
  return Images
    { imagePers   = scale 3 3 pers
    , imageBackground = scale 3 3 bgrd 
    , imageGameOver = scale 3 3 gover
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
  -- , drawGameOver (imageGameOver images)
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
  | isOnPlatform u = (upUniverse dt u) {universePlayer = keepPlayer dt (universePlayer u)}
  | isNearPlatform u = (upUniverse dt u) {universePlayer = updatePlayer dt (keepPlayerNearPlatform (universePlayer u))}
  | otherwise = (upUniverse dt u) {universePlayer = updatePlayer dt (universePlayer u)}
 
upUniverse:: Float -> Universe -> Universe 
upUniverse dt u = u { universePlatforms  = updatePlatforms  dt (universePlatforms  u)
      , universeScore  = (universeScore u) + dt
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
collides player platform = and [
  ((playerHeight player - heigthOfPlayer) < (snd platform + platformHeight)), 
  ((playerHeight player - heigthOfPlayer) > (snd platform)), 
  ((playerWidth player - widthOfPlayer) < (fst platform + platformWidth /2)), 
  ((playerWidth player + widthOfPlayer) > (fst platform - platformWidth /2))]
   where heigthOfPlayer = 1200*0.03
         widthOfPlayer = 800*0.03


-- | Упрощённая проверка на пересечение многоугольников.
polygonBoxCollides :: Path -> (Point, Point) -> Bool
polygonBoxCollides xs (lb, rt) = or
  [ not (segClearsBox p1 p2 lb rt)
  | (p1, p2) <- zip xs (tail (cycle xs))  ]

-- | Обновить состояние игрока.
-- Игрок не может прыгнуть выше потолка.
 -- | Обновить состояние игрока.

keepPlayerNearPlatform :: Player -> Player
keepPlayerNearPlatform player = player {
  playerSpeed = 0}

keepPlayerOnScreen :: Float -> Player -> Player 
keepPlayerOnScreen dt player = player {
  playerWidth = (max (min w (playerWidth player) + dt * (playerSpeed player)) wm)
} 
  where
    w = 200
    wm = -200 

keepPlayerOnPlatform :: Float -> Player -> Player
keepPlayerOnPlatform dt player = player {
   playerHeight = playerHeight player + dt * speed,
   playerFallingSpeed = 0
}

keepPlayer :: Float -> Player-> Player
keepPlayer dt player = keepPlayerOnScreen dt (keepPlayerOnPlatform dt player)

movePlayer :: Float -> Player -> Player
movePlayer dt player = player {
  playerFallingSpeed = (playerFallingSpeed player) + dt * gravity,
  playerHeight = (playerHeight player) + dt * (playerFallingSpeed player)
}

updatePlayer :: Float -> Player -> Player
updatePlayer dt player = (keepPlayerOnScreen dt (movePlayer dt player))

--updatePlayer :: Height -> Width -> Float -> Float -> Player -> Player
--updatePlayer h w fs s player = player
--  { playerHeight = h
--  , playerWidth = w
--  , playerFallingSpeed  = fs
--  , playerSpeed = s
--  }
