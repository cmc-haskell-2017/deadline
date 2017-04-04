module Handle where

import System.Random
import Graphics.Gloss.Geometry.Line
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Pure.Simulate
import Graphics.Gloss.Juicy
import Types
import Init


-- | Сбросить игру (начать с начала со случайными воротами).
resetUniverse :: StdGen -> Universe -> Universe
resetUniverse g _ = initUniverse g

-- | Подпрыгнуть (игроком), если можно.
bumpPlayerLeft :: Universe -> Universe
bumpPlayerLeft u = u
  { universePlayer = bump (universePlayer u)
  }
  where
    bump player = player {
    playerSpeed = -bumpSpeed }

bumpPlayerRight :: Universe -> Universe
bumpPlayerRight u = u
  { universePlayer = bump (universePlayer u)
  }
  where
    bump player = player {
    playerSpeed = bumpSpeed }

stopPlayer :: Universe -> Universe
stopPlayer u = u
  { universePlayer = bump (universePlayer u)
  }
  where
    bump player = player {
    playerSpeed = 0}

-- | Обработчик событий игры.
handleUniverse :: StdGen -> Event -> Universe -> Universe
handleUniverse _ (EventKey (SpecialKey KeyLeft) Down _ _) u = bumpPlayerLeft u
handleUniverse _ (EventKey (SpecialKey KeyRight) Down _ _) u = bumpPlayerRight u
handleUniverse _ (EventKey (SpecialKey KeyLeft) Up _ _) u = stopPlayer u
handleUniverse _ (EventKey (SpecialKey KeyRight) Up _ _) u = stopPlayer u
handleUniverse g (EventKey (SpecialKey KeySpace) Down _ _) u = resetUniverse g u
handleUniverse _ _ u = u