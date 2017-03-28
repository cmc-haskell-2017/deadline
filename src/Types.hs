module Types where
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Line
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
--type Score = Float
 -- deriving (Num, Eq, Show, Integral, Ord)

-- | Игрок
data Player = Player
  { playerWidth :: Width
  , playerHeight :: Height  -- ^ Положение игрока по вертикали.
  , playerSpeed :: Float
  , playerFallingSpeed  :: Float   -- ^ Скорость падения игрока.
  }

-- | Модель игровой вселенной.
data Universe = Universe
  { universePlatforms   :: [Platform]   -- ^ Платформы игровой вселенной.
  , universePlayer  :: Player   -- ^ Игрок
  , universeScore   :: Float    -- ^ Счёт (количество пролетевших мимо платформ)
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

-- | Положение верхнего края экрана
screenUp :: Offset
screenUp =  fromIntegral screenHeight / 2

-- | Положение нижнего края экрана
screenDown :: Offset
screenDown = - fromIntegral screenHeight / 2

-- | Ширина платформы
platformWidth :: Float
platformWidth = 120

-- | Высота платформы
platformHeight :: Float
platformHeight = 20

-- | Расстояние между платформами
defaultOffset :: Offset
defaultOffset = 200

-- | Диапазон генерации платформ
platformWidthRange :: (Width, Width)
platformWidthRange = (-w, w)
  where
    w = (fromIntegral screenWidth - platformWidth) / 2

-- | Параметры платформы
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
