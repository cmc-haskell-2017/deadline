module Deadline where

import System.Random
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Juicy
import Types
import Draw
import Init
import Handle
import Update

-- | Запустить игру «Deadline».
runDeadline :: Images -> String -> String -> Int -> IO ()
runDeadline images name id score = do
  g <- newStdGen
  universe <- (pure (initUniverse g name id score))
  playIO display bgColor fps universe (drawUniverse images) handleUniverse updateUniverse
  where
    display = InWindow "DEADLINE" (screenWidth, screenHeight) (200, 200)
    bgColor = white   -- цвет фона
    fps     = 60      -- кол-во кадров в секунду

-- | Загрузить изображения из файлов.
loadImages :: IO Images
loadImages = do
  Just pers   <- loadJuicyPNG "src/person.png"
  Just bgrd   <- loadJuicyPNG "src/background.png"
  Just gover  <- loadJuicyPNG "src/gameover.png"
  return Images
    { imagePers   = scale 3 3 pers
    , imageBackground1 = scale 3 3 bgrd 
    , imageBackground2 = scale 3 3 bgrd 
    , imageGameOver = scale 3 3 gover
    }
