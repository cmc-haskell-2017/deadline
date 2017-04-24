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
  { universePlatforms  = absolutePlatforms (initPlatforms g)
  , universePlayer = initPlayer
  , universeScore  = 0
  , universeBackground = initBackground
  , universeGameOver = Nothing
  , universeCannon = initCannon
  }

-- | Создать бесконечный список платформ.
absolutePlatforms :: [Platform] -> [Platform]
absolutePlatforms = go 0  
  where
    go  _ [] = []
    go  s ((w, o, t) : gs) = (w, s - o, t) : (go (s - o) gs)

-- | Инициализировать начальное состояние игрока.
initPlayer :: Player
initPlayer = Player
  { playerHeight = 300
  , playerWidth = 0
  , playerSpeed = 0
  , playerIsOnPlatform = False
  , playerFallingSpeed  = 0
  }

-- | Инициализировать одну платформу.
initPlatform :: Width -> Platform
initPlatform h = (h, defaultOffset, timeOfLife)

-- | Инициализировать случайный бесконечный
-- список платформ для игровой вселенной.
initPlatforms :: StdGen -> [Platform]
initPlatforms g = map initPlatform
  (randomRs platformWidthRange g)

initBackground :: Background
initBackground = Background
  { bgHeight1 = bgHeight / 2
  , bgHeight2 = -bgHeight / 2
  , bgSpeed = 70
  }
-- | Инициализировать конец игры.
initGameOver :: Point
initGameOver = (0.32, 0.32)

-- | Инициализировать пушку.
initCannon :: Cannon
initCannon = Cannon
  { cannonWidth = 0
  , cannonRecharge = 0
  , cannonBullets = []
  }
