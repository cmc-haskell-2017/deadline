module Handle where

import System.Random
import Graphics.Gloss.Geometry.Line
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Pure.Simulate
import Graphics.Gloss.Juicy
import Types
import Init
import Update
import Database
import qualified Data.Text as T

-- | Сдвинуть игрока влево.
bumpPlayerLeft :: Universe -> Universe
bumpPlayerLeft u = u
  { universePlayer = bump (universePlayer u)
  }
  where
    bump player = player {
    playerSpeed = -bumpSpeed }

-- | Сдвинуть игрока вверх.
bumpPlayerUp :: Universe -> Universe
bumpPlayerUp u = u
  { universePlayer = bump (universePlayer u)
  }
  where
    bump player = player {
    playerFallingSpeed = jumpSpeed }

-- |Сдвинуть игрока вправо.
bumpPlayerRight :: Universe -> Universe
bumpPlayerRight u = u
  { universePlayer = bump (universePlayer u)
  }
  where
    bump player = player {
    playerSpeed = bumpSpeed }

-- | Остановить игрока.
stopPlayer :: Universe -> Universe
stopPlayer u = u
  { universePlayer = bump (universePlayer u)
  }
  where
    bump player = player {
    playerSpeed = 0}

-- | Выбрать первый элемент кортежа из трех элементов.
firstOfTuple :: Platform -> Int
firstOfTuple (x, y, z) = truncate x

printBlank :: IO ()
printBlank = do
  putStrLn "-----------------------------------"

printCurrentPlayer :: Universe -> IO ()
printCurrentPlayer u = do
  putStrLn ((Types.id u) ++ (" scored: " ++ (show (truncate (universeScore u)))) )

printPlayers :: [RankingRow] -> IO ()
printPlayers (player : []) = do 
  putStrLn (((T.unpack (rankingPlayerName player)) ++ "'s highscore: ") ++ (show (rankingPlayerScore player)))
printPlayers (player : ps) = do 
  putStrLn (((T.unpack (rankingPlayerName player)) ++ "'s highscore: ") ++ (show (rankingPlayerScore player)))
  printPlayers ps

-- | Обработчик событий игры.
handleUniverse :: Event -> Universe -> IO Universe
handleUniverse (EventKey (SpecialKey KeyLeft) Down _ _) u = pure (bumpPlayerLeft u)
handleUniverse (EventKey (SpecialKey KeyUp) Down _ _) u
   | (playerIsOnPlatform (universePlayer u)) = pure (bumpPlayerUp u)
   | otherwise = pure u
handleUniverse (EventKey (SpecialKey KeyRight) Down _ _) u = pure (bumpPlayerRight u)
handleUniverse (EventKey (SpecialKey KeyLeft) Up _ _) u = pure (stopPlayer u)
handleUniverse (EventKey (SpecialKey KeyRight) Up _ _) u = pure (stopPlayer u)
handleUniverse (EventKey (SpecialKey KeySpace) Down _ _) u  = do 
  printBlank
  printCurrentPlayer u
  record <- getRanking
  printPlayers record
  universeReturn <- (pure (initUniverse (mkStdGen (firstOfTuple (head (universePlatforms u)))) (Types.id u) (truncate (universeScore u))))
  return universeReturn
handleUniverse _ u = pure u
