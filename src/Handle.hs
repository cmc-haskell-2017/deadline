module Handle where

import System.Random
import Graphics.Gloss.Geometry.Line
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Pure.Simulate
import Graphics.Gloss.Juicy
import Types
import Init
import Update

-- | Сдвинуть игрока вверх.
bumpPlayerUp :: Universe -> Universe
bumpPlayerUp u = u
  { universePlayer = bump (universePlayer u)
  }
  where
    bump player = player {
    playerFallingSpeed = jumpSpeed }

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
firstOfTuple (x, _, _) = truncate x

-- | Сдвинуть игрока вправо/влево.
bumpPlayerLR :: Int -> Universe -> (Bool, Time) -> Universe
bumpPlayerLR 1 u (False, _) = u
  { universePlayer = bump (universePlayer u)}
  where
    bump player = player { playerSpeed = bumpSpeed }
bumpPlayerLR 1 u (True, _) = u
  { universePlayer = bump (universePlayer u)}
  where bump player = player { playerSpeed = bumpSpeed * 2 }
bumpPlayerLR 0 u (False, _) = u
  { universePlayer = bump (universePlayer u)}
  where
    bump player = player { playerSpeed = -bumpSpeed }
bumpPlayerLR 0 u (True, _) = u
  { universePlayer = bump (universePlayer u)}
  where bump player = player { playerSpeed = -bumpSpeed * 2 }

-- |Сдвинуть игрока вправо.
bumpPlayerRight :: Universe -> Universe
bumpPlayerRight u = u
  { universePlayer = bump (universePlayer u)
  }
  where
    bump player = player {
    playerSpeed = bumpSpeed }

-- | Обработчик событий игры.
handleUniverse :: Event -> Universe -> Universe
handleUniverse (EventKey (SpecialKey KeyLeft) Down _ _) u = bumpPlayerLR 0 u (bonusCoffeeInt(universePlayer u))
handleUniverse (EventKey (SpecialKey KeyRight) Down _ _) u = bumpPlayerLR 1 u (bonusCoffeeInt(universePlayer u))
handleUniverse (EventKey (SpecialKey KeyLeft) Up _ _) u = stopPlayer u
handleUniverse (EventKey (SpecialKey KeyRight) Up _ _) u = stopPlayer u
handleUniverse (EventKey (SpecialKey KeyUp) Down _ _) u
   | (fst (bonusRedBullInt (universePlayer u))) = (playerFly u 1 1)
   | (playerIsOnPlatform (universePlayer u)) = bumpPlayerUp u
   | otherwise = u
handleUniverse (EventKey (SpecialKey KeyDown) Down _ _) u
   | (fst (bonusRedBullInt (universePlayer u))) = (playerFly u 0 1)
   | otherwise = u
handleUniverse (EventKey (SpecialKey KeyUp) Up _ _) u
   | (fst (bonusRedBullInt (universePlayer u))) = (playerFly u 0 0)
   | otherwise = u
handleUniverse (EventKey (SpecialKey KeyDown) Up _ _) u
   | (fst (bonusRedBullInt (universePlayer u))) = (playerFly u 0 0)
   | otherwise = u
handleUniverse (EventKey (SpecialKey KeySpace) Down _ _) u  = initUniverse (mkStdGen (firstOfTuple (head (universePlatforms u))))
handleUniverse _ u = u

-- Управление вверх-вниз
playerFly :: Universe -> Int -> Int -> Universe
playerFly u 1 1 = u {universePlayer = (playerFlyUp (universePlayer u))}
  where
    playerFlyUp p = p {
      playerFallingSpeed = 300
    }
playerFly u 0 1 = u {universePlayer = (playerFlyDown (universePlayer u))}
  where
    playerFlyDown p = p {
      playerFallingSpeed = (-300)
    }
playerFly u 0 0 = u {universePlayer = (playerFlyDown (universePlayer u))}
  where
    playerFlyDown p = p {
      playerFallingSpeed = (0)
    }

