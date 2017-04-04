module Init where

import System.Random
import Graphics.Gloss.Geometry.Line
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Pure.Simulate
import Graphics.Gloss.Juicy
import Types

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
