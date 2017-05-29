module Types where
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Line
import Graphics.Gloss.Interface.Pure.Game

-- | Ширина. 
type Width = Float

-- | Высота.
type Height = Float

-- | Положение блока (по горизонтали).
type Offset = Float

-- | Жизнь платформы.
type Life = Float

-- | Платформа.
type Platform   = (Width, Offset, Life)

-- | Вектор.
type Vector = (Int, Int)

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
  } 

-- | Игрок.
data Player = Player
  { playerWidth :: Width          -- ^ Положение игрока по горизонтали.
  , playerHeight :: Height        -- ^ Положение игрока по вертикали.
  , playerSpeed :: Float          -- ^ Скорость движения игрока по горизонтали.
  , playerFallingSpeed :: Float   -- ^ Скорость падения игрока.
  , playerIsOnPlatform :: Bool
  , playerStrategy :: Strategy
  }


-- | Модель игровой вселенной.
data Universe = Universe
  { universePlatforms   :: [Platform]   -- ^ Платформы игровой вселенной.
  , universePlayer  :: Player           -- ^ Игрок
  , universeScore   :: Float            -- ^ Счёт (количество пролетевших мимо платформ)
  , universeBackground :: Background
  , universeGameOver :: Maybe Point
  , universeCannon :: Cannon
  , universePlay :: Bool
  , universeRobot :: Player
  , time :: Float
  }

-- | Изображения объектов.
data Images = Images
  { imagePers  :: Picture   -- ^ Изображение персонажа.
  , imageRobot :: Picture
  , imageBackground1  :: Picture
  , imageBackground2  :: Picture
  , imageGameOver :: Picture
  , imageCannon :: Picture
  , imageBullets :: Picture
  , imagePlat :: Picture
  }

-- | Пушка.
data Cannon = Cannon
  { cannonWidth :: Width
  , cannonRecharge :: Float
  , cannonBullets :: [Bullet]
  }

-- | Пуля.
data Bullet = Bullet
  { bulletWidth :: Width
  , bulletHeight :: Height
  }

-- | Виды стратегии.
data EnumStrategy = OnPlatform | FromPlatform | FromBullet | No deriving (Eq)

-- | Скорость пули.
bulletSpeed :: Float
bulletSpeed = 600

-- | Скорость пушки.
cannonNormSpeed :: Float
cannonNormSpeed = 100

-- | Время перезарядки.
timeOfRecharge :: Float
timeOfRecharge = 30.0

-- | Высота игрока.
heightOfPlayer :: Float
heightOfPlayer = 1200 * 0.03

-- | Ширина игрока.
widthOfPlayer :: Float
widthOfPlayer = 800 * 0.03

-- | Ширина пули.
bulletsWidth :: Float
bulletsWidth = 590 * 0.01

-- | Высота пули.
bulletsHeight :: Float
bulletsHeight = 297 * 0.02

-- | Ширина экрана.
screenWidth :: Int
screenWidth = 900

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
platformWidth = 100

-- | Высота платформы.
platformHeight :: Float
platformHeight = 20

-- Расстояние между платформами.
defaultOffset :: Offset
defaultOffset = 150

bgHeight :: Float
bgHeight = 7900

-- | Диапазон генерации платформ.
platformWidthRange :: (Width, Width)
platformWidthRange = (-w, w)
  where
    w = (fromIntegral screenWidth - platformWidth) / 2

-- | Параметры платформы.
platformBoxes :: Platform -> [(Point, Point)]
platformBoxes (x, y, _) = [((x - w, y), (x + w, y + h))]
  where
    w = platformWidth / 2
    h = platformHeight

-- | Скорость движения игрока по вселенной (в пикселях в секунду).
speed :: Float
speed = 300

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
timeOfLife = 1.0

-- | Включен ли персонаж.
livePlayer :: Bool
livePlayer = False

-- | Надо ли нарисовать платформу, на которую предполагает попасть ИИ.
testDraw :: Bool
testDraw = False

-- | Стратегия со вспомогательными параметрами.
data Strategy = Strategy 
  { enumStrategy :: EnumStrategy
  , strategyPlatform :: Platform
  , falseTransition :: Bool
  }
