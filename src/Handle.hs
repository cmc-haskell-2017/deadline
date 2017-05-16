module Handle where

import System.Random
import Graphics.Gloss.Geometry.Line
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Pure.Simulate
import Graphics.Gloss.Juicy
import Types
import Init
import Update
import Collides

-- | Сдвинуть игрока влево.
bumpPlayerLeft :: Universe -> Universe
bumpPlayerLeft u 
  | collidesHelper (rotateLeft (playerSquare (universePlayer u) (time u))) (rotateLeft (playerSquare (universeRobot u) (time u))) = u
    { universePlayer = (universePlayer u) { playerSpeed = 0.0}}
  | otherwise = u
    { universePlayer = bump (universePlayer u)
    }
  where
    bump player = player {
    playerSpeed = -bumpSpeed }

-- | Сдвинуть игрока вверх.
bumpPlayerUp :: Universe -> Universe
bumpPlayerUp u = u
  { universePlayer = bump (universePlayer u)}
  where
    bump player = player {
    playerFallingSpeed = jumpSpeed }

-- |Сдвинуть игрока вправо.
bumpPlayerRight :: Universe -> Universe
bumpPlayerRight u 
  | collidesHelper (rotateRight (playerSquare (universePlayer u) (time u))) (rotateRight (playerSquare (universeRobot u) (time u))) = u
    { universePlayer = (universePlayer u) { playerSpeed = 0.0}}
  | otherwise = u
  { universePlayer = bump (universePlayer u)
  }
  where
    bump player = player {
    playerSpeed = bumpSpeed }

-- | Остановить игрока.
stopPlayer :: Universe -> Universe
stopPlayer u = u
  { universePlayer = bump (universePlayer u)
  }
  where
    bump player = player {
    playerSpeed = 0}

-- | Выбрать первый элемент кортежа из трех элементов.
firstOfTuple :: Platform -> Int
firstOfTuple (x, y, z) = truncate x

-- | Обработчик событий игры.
handleUniverse :: Event -> Universe -> Universe
handleUniverse (EventKey (SpecialKey KeyLeft) Down _ _) u = bumpPlayerLeft u
handleUniverse (EventKey (SpecialKey KeyUp) Down _ _) u
   | (playerIsOnPlatform (universePlayer u)) = bumpPlayerUp u
   | otherwise = u
handleUniverse (EventKey (SpecialKey KeyRight) Down _ _) u = bumpPlayerRight u
handleUniverse (EventKey (SpecialKey KeyLeft) Up _ _) u = stopPlayer u
handleUniverse (EventKey (SpecialKey KeyRight) Up _ _) u = stopPlayer u
handleUniverse (EventKey (SpecialKey KeySpace) Down _ _) u  = initUniverse (mkStdGen (firstOfTuple (head (universePlatforms u))))
handleUniverse _ u = u
