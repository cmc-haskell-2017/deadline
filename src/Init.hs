module Init where

import System.Random
import Graphics.Gloss.Geometry.Line
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Pure.Simulate
import Graphics.Gloss.Juicy
import Types

-- | Инициализировать игровую вселенную.
initUniverse :: StdGen -> Universe
initUniverse g = Universe
  { universePlatforms  = initPlatforms g
  , universePlayer = initPlayer
  , universeScore  = 0
  , universeBorders = 0
  , universeBackground = initBackground
  , universeGameOver = Nothing
  }

-- | Инициализировать начальное состояние игрока.
initPlayer :: Player
initPlayer = Player
  { playerHeight = 300
  , playerWidth = 0
  , playerSpeed = 0
  , playerFallingSpeed  = 0
  }

-- | Инициализировать одну платформу.
initPlatform :: Width -> Platform
initPlatform h = (h, defaultOffset)

-- | Инициализировать случайный бесконечный
-- список платформ для игровой вселенной.
initPlatforms :: StdGen -> [Platform]
initPlatforms g = map initPlatform
  (randomRs platformWidthRange g)

initBackground :: Background
initBackground = Background
  { bgHeight1 = 345
  , bgHeight2 = -345
  , bgSpeed = 50
  }
-- | Инициализировать конец игры.
initGameOver :: Point
initGameOver = (0.32, 0.32)
