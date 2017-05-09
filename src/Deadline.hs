module Deadline where

import System.Random
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy
import Types
import Draw
import Init
import Handle
import Update
--import Graphics.Image.Processing

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
  Just redbul <- loadJuicyPNG "src/bonus/redBull.png"
  Just coffee <- loadJuicyPNG "src/bonus/coffee.png"
  Just bottle <- loadJuicyPNG "src/bonus/bottle.png"
  Just speedicon <- loadJuicyPNG "src/bonus/speed.png"
  Just staricon <- loadJuicyPNG "src/bonus/star.png"
  return Images
    { imagePers   = scale 3 3 pers
    , imageBackground1 =  bgrd
    , imageBackground2 =  bgrd
    , imageGameOver = scale 3 3 gover
    , bonusRedBull = scale 0.4 0.4 redbul
    , bonusCoffee = scale 0.4 0.4 coffee
    , bonusBottle = scale 0.4 0.4 bottle
    , bonusSpeed = scale 0.4 0.4 speedicon
    , bonusStar = scale 0.4 0.4 staricon
    }
