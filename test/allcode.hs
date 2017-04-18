module Types where
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Line
import Graphics.Gloss.Interface.Pure.Game

type Width = Float
type Height = Float
type Offset = Float
type Life = Float
type Platform   = (Width, Offset, Life)
type Vector = (Int, Int)
data Square = Square
  { xCoordinateLeft :: Float,
    yCoordinateLeft :: Float,
    xCoordinateRight :: Float,
    yCoordinateRight :: Float,
    xSpeed :: Float,
    ySpeed :: Float
  }

data Background = Background
  { bgHeight1 :: Height
  , bgHeight2 :: Height
  , bgSpeed :: Float
data Player = Player
  { playerWidth :: Width          
  , playerHeight :: Height        
  , playerSpeed :: Float          
  , playerFallingSpeed :: Float   
  , playerIsOnPlatform :: Bool
  }

data Universe = Universe
  { universePlatforms   :: [Platform]   
  , universePlayer  :: Player           
  , universeScore   :: Float            
  , universeBackground :: Background
  , universeGameOver :: Maybe Point
  }
data Images = Images
  { imagePers  :: Picture   
  , imageBackground1  :: Picture
  , imageBackground2  :: Picture
  , imageGameOver :: Picture
  }
screenWidth :: Int
screenWidth = 450
screenHeight :: Int
screenHeight = 700
screenRight :: Offset
screenRight = fromIntegral screenWidth / 2
screenLeft :: Offset
screenLeft = - fromIntegral screenWidth / 2
screenUp :: Offset
screenUp =  fromIntegral screenHeight / 2
screenDown :: Offset
screenDown = - fromIntegral screenHeight / 2
platformWidth :: Float
platformWidth = 120
platformHeight :: Float
platformHeight = 20
defaultOffset :: Offset
defaultOffset = 200
platformWidthRange :: (Width, Width)
platformWidthRange = (-w, w)
  where
    w = (fromIntegral screenWidth - platformWidth) / 2
platformBoxes :: Platform -> [(Point, Point)]
platformBoxes (x, y, l) = [((x - w, y), (x + w, y + h))]
  where
    w = platformWidth / 2
    h = platformHeight
speed :: Float
speed = 200

jumpSpeed :: Float
jumpSpeed = 480
bumpSpeed :: Float
bumpSpeed = 400
playerOffset :: Offset
playerOffset = screenLeft + 200
gravity :: Float
gravity = -970
timeOfLife :: Float
timeOfLife = 1.0

module Init where

import System.Random
import Graphics.Gloss.Geometry.Line
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Pure.Simulate
import Graphics.Gloss.Juicy
import Types
initUniverse :: StdGen -> Universe
initUniverse g = Universe
  { universePlatforms  = absolutePlatforms (initPlatforms g)
  , universePlayer = initPlayer
  , universeScore  = 0
  , universeBackground = initBackground
  , universeGameOver = Nothing
  }
absolutePlatforms :: [Platform] -> [Platform]
absolutePlatforms = go 0  
  where
    go  _ [] = []
    go  s ((w, o, t) : gs) = (w, s - o, t) : (go (s - o) gs)
initPlayer :: Player
initPlayer = Player
  { playerHeight = 300
  , playerWidth = 0
  , playerSpeed = 0
  , playerIsOnPlatform = False
  , playerFallingSpeed  = 0
  }
initPlatform :: Width -> Platform
initPlatform h = (h, defaultOffset, timeOfLife)

initPlatforms :: StdGen -> [Platform]
initPlatforms g = map initPlatform
  (randomRs platformWidthRange g)

initBackground :: Background
initBackground = Background
  { bgHeight1 = 350
  , bgHeight2 = -350
  , bgSpeed = 100
initGameOver :: Point
initGameOver = (0.32, 0.32)

module Draw where

import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Interface.Pure.Game
import Types
drawPlatforms :: [Platform] -> Picture
drawPlatforms = pictures . map drawPlatform . takeWhile onScreen
  where
    onScreen (_, offset,_) = offset - platformHeight > screenDown
drawPlatform :: Platform -> Picture
drawPlatform p = (pictures [ color (makeColorI 0 66 72 255) (pictures (map drawBox  (platformBoxes p)))
                           , color (makeColorI 0 120 170 255) (pictures (map drawBox1 (platformBoxes p)))
                           , color (makeColorI 0 66 72 255) (pictures (map drawBox2  (platformBoxes p)))
                           , color (makeColorI 0 66 72 255) (pictures (map drawBox3  (platformBoxes p))) ])
  where
    drawBox ((l, b), (r, t)) = polygon
      [ (l, b), (r, b), (r, t), (l, t) ]
    drawBox1 ((l, b), (r, t)) = polygon
      [ (l+3, b+3), (r-3, b+3), (r-3, t-3), (l+3, t-3) ]
    drawBox2 ((l, b), (r, t)) = polygon
      [ (l, b+3), (l+3, b), (r, t-3), (r-3, t) ]
    drawBox3 ((l, b), (r, t)) = polygon
      [ (l, t-3), (l+3, t), (r, b+3), (r-3, b) ]

drawPlayer :: Picture -> Player -> Picture
drawPlayer image player = translate x y (scale 0.1075 0.1075 image)
  where
    (x, y) = (playerWidth player, playerHeight player)
drawBackground :: Picture -> Picture -> Background -> Picture
drawBackground bg1 bg2 bg = pictures [ (translate 1 y1 bg1), (translate 1 y2 bg2)]
    where
      (y1, y2) = (bgHeight1 bg, bgHeight2 bg)
drawBorders :: Picture
drawBorders = translate (-w) h (scale 30 30 (pictures
  [ color red (polygon [ (0, 0), (0, -2), (15, -2), (15, 0) ])            -- верхняя граница
  , color red (polygon [ (0, -21.5), (0, -24), (15, -24), (15, -21.5) ]) -- нижняя граница
  ]))
  where
    w = fromIntegral screenWidth / 2
    h = fromIntegral screenHeight / 2
drawGameOver :: Picture -> Maybe Point -> Picture
drawGameOver _ Nothing = blank
drawGameOver image (Just (x, y)) = (scale x y image)
drawText :: Int -> Float -> Float -> String -> Picture
drawText k w h s = translate (-sw) sh (scale 30 30 (pictures (drawTextList k w h s)))
  where
    sw = fromIntegral screenWidth / 2
    sh = fromIntegral screenHeight / 2
drawTextList :: Int -> Float -> Float -> String -> [Picture]
drawTextList 0 _ _ _ = []
drawTextList k w h s = (drawTextFunc w h s) : (drawTextList (k-1) (w+0.02) (h+0.01) s) 
drawTextFunc :: Float -> Float -> String -> Picture
drawTextFunc w h s = translate w (h) (scale 0.01 0.01 (color black (text s)))
drawUniverse :: Images -> Universe -> Picture
drawUniverse images u = pictures
  [ drawBackground (imageBackground1 images) (imageBackground2 images) (universeBackground u)
  , drawPlatforms  (universePlatforms u)
  , pictures (map (drawPlayer (imagePers images)) [ (universePlayer u) ] ) 
  , drawBorders
  , drawText 5 4.5 (-1.5) "DEADLINE"
  , drawText 5 4.35 (-22.9) "exhaustion"
  , drawText 5 1 (-1.5) (show (truncate (universeScore u)))
  , drawGameOver (imageGameOver images) (universeGameOver u)
  ]

module Handle where

import System.Random
import Graphics.Gloss.Geometry.Line
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Pure.Simulate
import Graphics.Gloss.Juicy
import Types
import Init
import Update
bumpPlayerLeft :: Universe -> Universe
bumpPlayerLeft u = u
  { universePlayer = bump (universePlayer u)
  }
  where
    bump player = player {
    playerSpeed = -bumpSpeed }
bumpPlayerUp :: Universe -> Universe
bumpPlayerUp u = u
  { universePlayer = bump (universePlayer u)
  }
  where
    bump player = player {
    playerFallingSpeed = jumpSpeed }
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
firstOfTuple :: Platform -> Int
firstOfTuple (x, y, z) = truncate x
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

updateUniverse :: Float -> Universe -> Universe
updateUniverse dt u
  | isGameOver u = u { universeGameOver = Just initGameOver }
  | fst (isWithPlatform dt u) = (upUniverse dt u) {universePlayer = keepPlayer dt (universePlayer u)}
  | snd (isWithPlatform dt u) = (upUniverse dt u) {universePlayer = holdPlayer dt (universePlayer u)}
  | otherwise = (upUniverse dt u) {universePlayer = updatePlayer dt (universePlayer u)}
 
upUniverse:: Float -> Universe -> Universe 
upUniverse dt u = u { universePlatforms  = updatePlatforms  dt (universePlatforms  u)(universePlayer u)
      , universeScore  = (universeScore u) + dt
      , universeBackground = updateBackground dt (universeBackground u)
      }

isWithPlatform :: Float -> Universe -> (Bool, Bool)
isWithPlatform dt u = playerWithPlatform
  where
    playerWithPlatform = (collision dt (universePlayer u) (universePlatforms u))

isGameOver :: Universe -> Bool
isGameOver u = playerBelowFloor || playerBelowRoof
  where
    playerBelowRoof = playerHeight (universePlayer u) >  screenUp - 30
    playerBelowFloor = playerHeight (universePlayer u) < screenDown + 30

collision :: Float -> Player -> [Platform] -> (Bool, Bool)
collision _ _ [] = (False, False)
collision dt player platforms = tupleOr (map (collides dt player) (takeWhile onScreen platforms))
  where
    onScreen (_, offset, _) = offset - platformHeight > screenDown

tupleOrFirst :: [(Bool, Bool)] -> Bool
tupleOrFirst [] = False
tupleOrFirst list = or (map fst list)

tupleOrSecond :: [(Bool, Bool)] -> Bool
tupleOrSecond [] = False
tupleOrSecond list = or (map snd list)

tupleOr :: [(Bool, Bool)] -> (Bool, Bool)
tupleOr [] = (False, False)
tupleOr list = ((tupleOrFirst list), (tupleOrSecond list))

rotateLeft :: Square -> Square
rotateLeft square = Square {
                  yCoordinateRight = -(xCoordinateRight square),
                  yCoordinateLeft = -(xCoordinateLeft square),
                  xCoordinateLeft = -(yCoordinateLeft square),
                  xCoordinateRight = -(yCoordinateRight square),
                  xSpeed = ySpeed square,
                  ySpeed = - (xSpeed square)
                }

rotateRight :: Square -> Square
rotateRight square = Square {
                  yCoordinateRight = (xCoordinateLeft square),
                  yCoordinateLeft = (xCoordinateRight square),
                  xCoordinateLeft = (yCoordinateRight square),
                  xCoordinateRight = (yCoordinateLeft square),
                  xSpeed = ySpeed square,
                  ySpeed = xSpeed square
                }

collides :: Float -> Player -> Platform -> (Bool, Bool)
collides dt player (width, offset, life) = ((collidesHelper (playerSquare player dt) (platformSquare (width, offset, life) dt)), 
  or [(collidesHelper (rotateLeft (playerSquare player dt)) (rotateLeft (platformSquare (width, offset, life) dt))), 
  (collidesHelper (rotateRight (playerSquare player dt)) (rotateRight (platformSquare (width, offset, life) dt)))])

collidesHelper :: Square -> Square -> Bool
collidesHelper player platform = 
  (and [(yCoordinateRight player > yCoordinateRight platform),
  (yCoordinateRight player + ySpeed player < yCoordinateLeft platform + ySpeed platform), 
  (yCoordinateRight player + platformHeight/4 > yCoordinateLeft platform), 
  (xCoordinateLeft player < xCoordinateRight platform), 
  (xCoordinateRight player > xCoordinateLeft platform)])

keepPlayerOnScreen :: Float -> Player -> Player 
keepPlayerOnScreen dt player = player {
  playerWidth = (max (min w (playerWidth player) + dt * (playerSpeed player)) wm)
} 
  where
    w = 200
    wm = -200 

keepPlayerOnPlatform :: Float -> Player -> Player
keepPlayerOnPlatform dt player = player {
   playerFallingSpeed = speed,
   playerIsOnPlatform = True,
   playerHeight = playerHeight player + dt * speed
}

holdPlayerOnPlatform :: Float -> Player -> Player
holdPlayerOnPlatform dt player = player {
   playerSpeed = 0,
   playerIsOnPlatform = False,
   playerFallingSpeed = (playerFallingSpeed player) + dt * gravity,
   playerHeight = (playerHeight player) + dt * ((playerFallingSpeed player) + dt * (gravity / 2))
}

keepPlayer :: Float -> Player-> Player
keepPlayer dt player = keepPlayerOnScreen dt (keepPlayerOnPlatform dt player)

holdPlayer :: Float -> Player-> Player
holdPlayer dt player = keepPlayerOnScreen dt (holdPlayerOnPlatform dt player)

movePlayer :: Float -> Player -> Player
movePlayer dt player = player {
  playerFallingSpeed = (playerFallingSpeed player) + dt * gravity,
  playerIsOnPlatform = False,
  playerHeight = (playerHeight player) + dt * ((playerFallingSpeed player) + dt * (gravity / 2))
}

updatePlatforms :: Float -> [Platform] -> Player -> [Platform]
updatePlatforms _ [] _ = []
updatePlatforms dt ((width, offset, time) : platforms) player
  | screenUp < offset = updatePlatforms dt platforms player
  | time - dt < 0 = updatePlatforms dt platforms player
  | collidesHelper (playerSquare player dt) (platformSquare (width, offset, time) dt) = (width, offset + dy, time - dt) : (updatePlatforms dt platforms player)
  | otherwise = (width, offset + dy, time) : (updatePlatforms dt platforms player)
  where
        dy  = dt * speed

playerSquare :: Player -> Float -> Square
playerSquare player dt = Square {
          xCoordinateLeft = (playerWidth player - widthOfPlayer),
          yCoordinateRight = (playerHeight player - heigthOfPlayer), 
          xCoordinateRight = (playerWidth player + widthOfPlayer),
          yCoordinateLeft = (playerHeight player + heigthOfPlayer), 
          xSpeed = dt * (playerSpeed player),
          ySpeed = dt * (playerFallingSpeed player)
}
  where
        heigthOfPlayer = 1200 * 0.03
        widthOfPlayer = 800 * 0.03

platformSquare :: Platform -> Float -> Square
platformSquare (width, offset, time) dt = Square {
          xCoordinateLeft = width - platformWidth /2,
          yCoordinateRight = offset, 
          xCoordinateRight = width + platformWidth /2,
          yCoordinateLeft = offset + platformHeight, 
          xSpeed = 0,
          ySpeed = speed * dt
}

updatePlayer :: Float -> Player -> Player
updatePlayer dt player = (keepPlayerOnScreen dt (movePlayer dt player))

updateBackground :: Float -> Background -> Background
updateBackground dt bg
  | (bgHeight1 bg) >= 700 = bg {
  bgHeight1 = -700,
  bgHeight2 = (bgHeight2 bg) + dt * (bgSpeed bg)
}
  | (bgHeight2 bg) >= 700 = bg {
  bgHeight1 = (bgHeight1 bg) + dt * (bgSpeed bg),
  bgHeight2 = -700
}
  | otherwise = bg {
  bgHeight1 = (bgHeight1 bg) + dt * (bgSpeed bg),
  bgHeight2 = (bgHeight2 bg) + dt * (bgSpeed bg)
}

module Deadline where

import System.Random
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy
import Types
import Draw
import Init
import Handle
import Update
runDeadline :: Images -> IO ()
runDeadline images = do
  g <- newStdGen
  play display bgColor fps (initUniverse g) (drawUniverse images) handleUniverse updateUniverse
  where
    display = InWindow "DEADLINE" (screenWidth, screenHeight) (200, 200)
    bgColor = white   
    fps     = 60      
loadImages :: IO Images
loadImages = do
  Just pers   <- loadJuicyPNG "src/person.png"
  Just bgrd   <- loadJuicyPNG "src/b2.png"
  Just gover  <- loadJuicyPNG "src/gameover.png"
  return Images
    { imagePers   = scale 3 3 pers
    , imageBackground1 =  bgrd 
    , imageBackground2 =  bgrd 
    , imageGameOver = scale 3 3 gover
    }