module Types where
import Graphics.Gloss.Interface.Pure.Game

-- | Ширина 
type Width = Float

-- | Высота
type Height = Float

-- | Положение блока (по горизонтали).
type Offset = Float

-- | Ворота.
type Platform   = (Width, Offset)

-- | Счёт.
type Score = Int

-- | Игрок — символ лямбда.
data Player = Player
  { playerWidth :: Width
  , playerHeight :: Height  -- ^ Положение игрока по вертикали.
  , playerSpeed :: Float
  , isOnPlatformNow :: Bool
  , isNearPlatformNow :: Bool
  , playerFallingSpeed  :: Float   -- ^ Скорость падения игрока.
  }

-- | Модель игровой вселенной.
data Universe = Universe
  { universePlatforms   :: [Platform]   -- ^ Ворота игровой вселенной.
  , universePlayer  :: Player   -- ^ Игрок.
  , universeScore   :: Score    -- ^ Счёт (кол-во успешно пройденных ворот).
  , universeBorders :: Int
  , universeBackground :: Int
  }

-- | Изображения объектов.
data Images = Images
  { imagePers  :: Picture   -- ^ Изображение персонажа.
  , imageBackground  :: Picture
  }

-- | Ширина экрана.
screenWidth :: Int
screenWidth = 450

-- | Высота экрана.
screenHeight :: Int
screenHeight = 700

-- | Положение правого края экрана.
screenRight :: Offset
screenRight = fromIntegral screenWidth / 2

-- | Положение левого края экрана.
screenLeft :: Offset
screenLeft = - fromIntegral screenWidth / 2

screenUp :: Offset
screenUp =  fromIntegral screenHeight / 2

screenDown :: Offset
screenDown = - fromIntegral screenHeight / 2

-- | Ширина стенок ворот.
platformWidth :: Float
platformWidth = 120

platformHeight :: Float
platformHeight = 20

-- | Размер проёма ворот.
platformSize :: Float
platformSize = 150

-- | Расстояние между воротами.
defaultOffset :: Offset
defaultOffset = 200

-- | Диапазон высот ворот.
platformWidthRange :: (Width, Width)
platformWidthRange = (-w, w)
  where
    w = (fromIntegral screenWidth - platformWidth) / 2

platformBoxes :: Platform -> [(Point, Point)]
platformBoxes (x, y) = [((x - w, y), (x + w, y + h))]
  where
    w = platformWidth / 2
    h = platformHeight

-- | Скорость движения игрока по вселенной (в пикселях в секунду).
speed :: Float
speed = 100

-- | Положение игрока по горизонтали.
playerOffset :: Offset
playerOffset = screenLeft + 200

-- | Ускорение свободного падения.
gravity :: Float
gravity = -970