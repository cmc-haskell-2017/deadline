module Types where
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Line
import Graphics.Gloss.Interface.Pure.Game

-- | Время
type Time = Float

-- | Ширина. 
type Width = Float

-- | Высота.
type Height = Float

-- | Положение блока (по горизонтали).
type Offset = Float

-- | Жизнь платформы.
type Life = Time

-- | Платформа.
type Platform   = (Width, Offset, Life)

-- | Вектор.
type Vector = (Int, Int)

-------------------------

type Bonus = (Width, Offset)

type PlayerBonus = (Bool, Time)

-- ---------------------

-- | Прямоугольник (игрок или платформа).
data Square = Square
  { xCoordinateLeft :: Float,
    yCoordinateLeft :: Float,
    xCoordinateRight :: Float,
    yCoordinateRight :: Float,
    xSpeed :: Float,
    ySpeed :: Float
  }

data Background = Background
  { bgHeight1 :: Float
  , bgHeight2 :: Float
  , bgSpeed :: Float
  , bgSize :: Float
  }

-- | Игрок.
data Player = Player
  { playerWidth :: Width          -- ^ Положение игрока по горизонтали.
  , playerHeight :: Height        -- ^ Положение игрока по вертикали.
  , playerSpeed :: Float          -- ^ Скорость движения игрока по горизонтали.
  , playerFallingSpeed :: Float   -- ^ Скорость падения игрока.
  , playerIsOnPlatform :: Bool
  , bonusBottleInt :: PlayerBonus
  , bonusSpeedInt :: PlayerBonus
  , bonusRedBullInt :: PlayerBonus
  , bonusCoffeeInt :: PlayerBonus
  }

-- | Модель игровой вселенной.
data Universe = Universe
  { universePlatforms   :: [Platform]   -- ^ Платформы игровой вселенной.
  , universePlayer  :: Player           -- ^ Игрок
  , universeScore   :: Float            -- ^ Счёт (количество пролетевших мимо платформ)
  , universeBackground :: Background
  , universeGameOver :: Maybe Point
  , universeBonusBottle :: [Bonus]
  , universeBonusStar :: [Bonus]
  , universeBonusSpeed :: [Bonus]
  , universeBonusCoffee :: [Bonus]
  , universeBonusRedBull :: [Bonus]
  }

-- | Изображения объектов.
data Images = Images
  { imagePers  :: Picture   -- ^ Изображение персонажа.
  , imageBackground1  :: Picture
  , imageBackground2  :: Picture
  , imageGameOver :: Picture
  , bonusRedBull :: Picture
  , bonusBottle :: Picture
  , bonusSpeed :: Picture
  , bonusCoffee :: Picture
  , bonusStar :: Picture
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

-- | Положение верхнего края экрана.
screenUp :: Offset
screenUp =  fromIntegral screenHeight / 2

-- | Положение нижнего края экрана.
screenDown :: Offset
screenDown = - fromIntegral screenHeight / 2

-- | Ширина платформы.
platformWidth :: Float
platformWidth = 120

-- | Высота платформы.
platformHeight :: Float
platformHeight = 20

-- | Расстояние между платформами.
defaultOffset :: Offset
defaultOffset = 200

--------------------------------

bonusSize :: Float
bonusSize = 40

bottlesOffset :: Offset
bottlesOffset = 4901

coffeeOffset :: Offset
coffeeOffset = 7312

redBullOffset :: Offset
redBullOffset = 3624

speedOffset  :: Offset
speedOffset  = 5678

starsOffset :: Offset
starsOffset = 4124

starBonusScore :: Float
starBonusScore = 50

speedOfBonus :: Float
speedOfBonus = 200

-- | Время действия бонуса
timeOfBonus :: Float
timeOfBonus = 5

------------------------------------

-- | Диапазон генерации платформ.
platformWidthRange :: (Width, Width)
platformWidthRange = (-w, w)
  where
    w = (fromIntegral screenWidth - platformWidth) / 2

-- | Параметры платформы.
platformBoxes :: Platform -> [(Point, Point)]
platformBoxes (x, y, l) = [((x - w, y), (x + w, y + h))]
  where
    w = platformWidth / 2
    h = platformHeight

jumpSpeed :: Float
jumpSpeed = 480

-- | Скорость после "подпрыгивания".
bumpSpeed :: Float
bumpSpeed = 400

-- | Положение игрока по горизонтали.
playerOffset :: Offset
playerOffset = screenLeft + 200

-- | Ускорение свободного падения.
gravity :: Float
gravity = -970

-- | Время жизни платформы.
timeOfLife :: Float
timeOfLife = 3

-- | Скорость движения игрока по вселенной (в пикселях в секунду).
speed :: Float
speed = 250
