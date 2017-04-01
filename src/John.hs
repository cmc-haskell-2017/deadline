module John where

import System.Random
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Line
import Graphics.Gloss.Interface.Pure.Game
import Types


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
