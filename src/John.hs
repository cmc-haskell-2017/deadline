module John where

import Graphics.Gloss.Interface.Pure.Game
import Types

-- | Обработчик событий игры.
handleUniverse :: Event -> Universe -> Universe
handleUniverse (EventKey (SpecialKey KeyLeft) Down _ _) = bumpPlayerLeft
handleUniverse (EventKey (SpecialKey KeyRight) Down _ _) = bumpPlayerRight
handleUniverse (EventKey (SpecialKey KeyUp) Down _ _) = bumpPlayerUp
handleUniverse (EventKey (SpecialKey KeyRight) Up _ _) = stopPlayer
handleUniverse (EventKey (SpecialKey KeyLeft) Up _ _) = stopPlayer

handleUniverse _ = id

-- | Подпрыгнуть (игроком), если можно.
bumpPlayerLeft :: Universe -> Universe
bumpPlayerLeft u = u
  { universePlayer = bump (universePlayer u)
  }
  where
    bump player = player {
    playerSpeed = -bumpSpeed }

-- | Подпрыгнуть (игроком), если можно.
bumpPlayerUp :: Universe -> Universe
bumpPlayerUp u = u
  { universePlayer = bump (universePlayer u)
  }
  where
    bump player = player {
    playerFallingSpeed = 400 }

bumpPlayerRight :: Universe -> Universe
bumpPlayerRight u = u
  { universePlayer = bump (universePlayer u)
  }
  where
    bump player = player {
    playerSpeed = bumpSpeed }

    -- | Подпрыгнуть (игроком), если можно.
stopPlayer :: Universe -> Universe
stopPlayer u = u
  { universePlayer = bump (universePlayer u)
  }
  where
    bump player = player {
    playerSpeed = 0}


-- | Скорость после "подпрыгивания".
bumpSpeed :: Float
bumpSpeed = 400