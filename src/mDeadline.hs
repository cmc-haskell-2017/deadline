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
  play display bgColor fps (initUniverse g) (drawUniverse images) (handleUniverse g) updateUniverse
  where
    display = InWindow "DEADLINE" (screenWidth, screenHeight) (200, 200)
    bgColor = white   -- цвет фона
    fps     = 60      -- кол-во кадров в секунду

-- | Загрузить изображения из файлов.
loadImages :: IO Images
loadImages = do
  Just pers   <- loadJuicyJPG "src/pers.jpg"
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
  , universeGameOver = Nothing
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

initGameOver :: Point
initGameOver = (0.32, 0.32)

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
  , drawGameOver (imageGameOver images) (universeGameOver u)
  ]

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

-- | Обновить состояние игровой вселенной.
updateUniverse :: Float -> Universe -> Universe
updateUniverse dt u
  | isGameOver u = u { universeGameOver = Just initGameOver } -- resetUniverse g u
  | isOnPlatform dt u = (upUniverse dt u) {universePlayer = keepPlayer dt (universePlayer u)}
  | isNextPlatform dt u = (upUniverse dt u) {universePlayer = updatePlayer dt (universePlayer u)}
  | otherwise = (upUniverse dt u) {universePlayer = updatePlayer dt (universePlayer u)}
 
upUniverse:: Float -> Universe -> Universe 
upUniverse dt u = u { universePlatforms  = updatePlatforms  dt (universePlatforms  u)
      , universeScore  = (universeScore u) + dt
      }

-- | Сбросить игру (начать с начала со случайными воротами).
resetUniverse :: StdGen -> Universe -> Universe
resetUniverse g _ = initUniverse g


isOnPlatform :: Float -> Universe -> Bool
isOnPlatform dt u = playerAbovePlatform
  where
    playerAbovePlatform = (((collision dt (universePlayer u) (universePlatforms u))!!0)!!1)

isNextPlatform :: Float -> Universe -> Bool
isNextPlatform dt u = playerNextToLeftPlatform || playerNextToRightPlatform
   where
    playerNextToLeftPlatform = (((collision dt (universePlayer u) (universePlatforms u))!!0)!!0)
    playerNextToRightPlatform = (((collision dt (universePlayer u) (universePlatforms u))!!0)!!2)

-- | Конец игры?
isGameOver :: Universe -> Bool
isGameOver u = playerBelowFloor || playerBelowRoof
  where
    playerBelowRoof = playerHeight (universePlayer u) >  screenUp - 30
    playerBelowFloor = playerHeight (universePlayer u) < screenDown + 30

-- | Сталкивается ли игрок с любыми из
-- бесконечного списка ворот?
collision :: Float -> Player -> [Platform] -> [[Bool]]
collision _ _ [] = [[False, False, False]]
collision dt player platforms = (map (collides dt player) (takeWhile onScreen (absolutePlatforms platforms)))
  where
    onScreen (_, offset) = offset - platformHeight > screenDown

-- |  Становится ли игрок на платформу?
collides :: Float -> Player -> Platform -> [Bool]
collides dt player platform = [(and [((playerHeight player - heigthOfPlayer + dt * (playerFallingSpeed player)) < (snd platform + platformHeight + dt * speed)),
  ((playerWidth player + widthOfPlayer + dt * (playerSpeed player)) < (fst platform - platformWidth /2))]),
  
  (and [((playerHeight player - heigthOfPlayer + dt * (playerFallingSpeed player)) < (snd platform + platformHeight + dt * speed)),
  ((playerHeight player - heigthOfPlayer - dt * (playerFallingSpeed player)) > (snd platform + dt * speed)), 
  ((playerWidth player - widthOfPlayer + dt * (playerSpeed player)) < (fst platform + platformWidth /2)), 
  ((playerWidth player + widthOfPlayer + dt * (playerSpeed player)) > (fst platform - platformWidth /2))]),

  (and [((playerHeight player - heigthOfPlayer + dt * (playerFallingSpeed player)) < (snd platform + platformHeight + dt * speed)),
  ((playerWidth player - widthOfPlayer + dt * (playerSpeed player)) > (fst platform + platformWidth /2))
  ])]
   where heigthOfPlayer = 1200 * 0.03
         widthOfPlayer = 800 * 0.03


-- | Упрощённая проверка на пересечение многоугольников.
polygonBoxCollides :: Path -> (Point, Point) -> Bool
polygonBoxCollides xs (lb, rt) = or
  [ not (segClearsBox p1 p2 lb rt)
  | (p1, p2) <- zip xs (tail (cycle xs))  ]

-- | Обновить состояние игрока.
-- Игрок не может прыгнуть выше потолка.
 -- | Обновить состояние игрока.

keepPlayerOnScreen :: Float -> Player -> Player 
keepPlayerOnScreen dt player = player {
  playerWidth = (max (min w (playerWidth player) + dt * (playerSpeed player)) wm)
} 
  where
    w = 200
    wm = -200 

keepPlayerOnPlatform :: Float -> Player -> Player
keepPlayerOnPlatform dt player = player {
   playerFallingSpeed = speed,
   playerHeight = playerHeight player + dt * speed
}

keepPlayer :: Float -> Player-> Player
keepPlayer dt player = keepPlayerOnScreen dt (keepPlayerOnPlatform dt player)

movePlayer :: Float -> Player -> Player
movePlayer dt player = player {
  playerFallingSpeed = (playerFallingSpeed player) + dt * gravity,
  playerHeight = (playerHeight player) + dt * ((playerFallingSpeed player) + dt * (gravity / 2))
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


-- | Обработчик событий игры.
handleUniverse :: StdGen -> Event -> Universe -> Universe
handleUniverse _ (EventKey (SpecialKey KeyLeft) Down _ _) u = bumpPlayerLeft u
handleUniverse _ (EventKey (SpecialKey KeyRight) Down _ _) u = bumpPlayerRight u
handleUniverse _ (EventKey (SpecialKey KeyLeft) Up _ _) u = stopPlayer u
handleUniverse _ (EventKey (SpecialKey KeyRight) Up _ _) u = stopPlayer u
handleUniverse g (EventKey (SpecialKey KeySpace) Down _ _) u = resetUniverse g u
handleUniverse _ _ u = u
