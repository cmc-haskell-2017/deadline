module Types where

-- | Высота (ворот или игрока).
type Height = Float

-- | Положение ворот (по горизонтали).
type Offset = Float

-- | Ворота.
type Gate   = (Offset, Height)

-- | Счёт.
type Score = Int

-- | Игрок — символ лямбда.
data Player = Player
  { playerHeight :: Height  -- ^ Положение игрока по вертикали.
  , playerSpeed  :: Float   -- ^ Скорость падения игрока.
  }

-- | Модель игровой вселенной.
data Universe = Universe
  { universeGates   :: [Gate]   -- ^ Ворота игровой вселенной.
  , universePlayer  :: Player   -- ^ Игрок.
  , universeScore   :: Score    -- ^ Счёт (кол-во успешно пройденных ворот).
  }
