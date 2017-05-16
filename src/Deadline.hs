module Deadline where

import System.Random
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy
import Types
import Draw
import Init
import Handle
import Update
import Cannon
import Collides
import AI
import TypesAI

-- | Запустить игру «Deadline».
runDeadline :: Images -> IO ()
runDeadline images = do
  g <- newStdGen
  play display bgColor fps (initUniverse g) (drawUniverse images) handleUniverse updateUniverse
  where
    display = InWindow "DEADLINE" (screenWidth, screenHeight) (200, 200)
    bgColor = white   -- цвет фона
    fps     = 60      -- кол-во кадров в секунду

-- | Загрузить изображения из файлов.
loadImages :: IO Images
loadImages = do
  Just pers   <- loadJuicyPNG "src/person.png"
  Just bgrd   <- loadJuicyPNG "src/bg.png"
  Just gover  <- loadJuicyPNG "src/gameover.png"
  Just cannon  <- loadJuicyPNG "src/cannon.png"
  Just bullets <- loadJuicyPNG "src/bullet.png"
  Just robot   <- loadJuicyPNG "src/robot.png"
  return Images
    { imagePers   = scale 3 3 pers
    , imageRobot = scale 3 3 robot
    , imageBackground1 =  bgrd 
    , imageBackground2 =  bgrd 
    , imageGameOver = scale 3 3 gover
    , imageCannon = scale 3 3 cannon
    , imageBullets = scale 3 3 bullets
    }

-- | Обновить состояние игровой вселенной.
updateUniverse :: Float -> Universe -> Universe
updateUniverse dt u
  | not(universePlay u) = u
  | (isGameOver dt (universePlayer u) u) || (isGameOver dt (universeRobot u) u) = u { universeGameOver = Just initGameOver
                        , universePlay = False }
  | fst (isWithPlatform dt (universePlayer u) u) = (upUniverse dt u) {universePlayer = keepPlayer dt (universePlayer u)}
  | snd (isWithPlatform dt (universePlayer u) u) = (upUniverse dt u) {universePlayer = holdPlayer dt (universePlayer u)}
  | otherwise = (upUniverse dt u) {universePlayer = updatePlayer dt (universePlayer u)}
 
-- | Обновление вселенной.
upUniverse:: Float -> Universe -> Universe 
upUniverse dt u = u { universePlatforms  = updatePlatforms  dt (universePlatforms  u) u
      , universeScore  = (universeScore u) + dt
      , universeBackground = updateBackground dt (universeBackground u)
      , universeCannon = updateCannon dt u
      , universeRobot = updateRobot dt u
      , time = dt
      }
