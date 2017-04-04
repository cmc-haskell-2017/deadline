module Update where

import Types
import Draw
import Init

-- | Обновить состояние игровой вселенной.
updateUniverse :: Float -> Universe -> Universe
updateUniverse dt u
  | isGameOver u = u { universeGameOver = Just initGameOver } -- resetUniverse g u
  | isOnPlatform dt u = (upUniverse dt u) {universePlayer = keepPlayer dt (universePlayer u)}
  | otherwise = (upUniverse dt u) {universePlayer = updatePlayer dt (universePlayer u)}
 
upUniverse:: Float -> Universe -> Universe 
upUniverse dt u = u { universePlatforms  = updatePlatforms  dt (universePlatforms  u)
      , universeScore  = (universeScore u) + dt
      }

isOnPlatform :: Float -> Universe -> Bool
isOnPlatform dt u = playerAbovePlatform
  where
    playerAbovePlatform = (collision dt (universePlayer u) (universePlatforms u))

-- | Конец игры?
isGameOver :: Universe -> Bool
isGameOver u = playerBelowFloor || playerBelowRoof
  where
    playerBelowRoof = playerHeight (universePlayer u) >  screenUp - 30
    playerBelowFloor = playerHeight (universePlayer u) < screenDown + 30

-- | Сталкивается ли игрок с любыми из
-- бесконечного списка ворот?
collision :: Float -> Player -> [Platform] -> Bool
collision _ _ [] = False
collision dt player platforms = or (map (collides dt player) (takeWhile onScreen (absolutePlatforms platforms)))
  where
    onScreen (_, offset) = offset - platformHeight > screenDown

-- |  Становится ли игрок на платформу?
collides :: Float -> Player -> Platform -> Bool
collides dt player platform = 
  (and [((playerHeight player - heigthOfPlayer + dt * (playerFallingSpeed player)) < (snd platform + platformHeight + dt * speed)),
  ((playerHeight player - heigthOfPlayer - dt * (playerFallingSpeed player)) > (snd platform + dt * speed)), 
  ((playerWidth player - widthOfPlayer + dt * (playerSpeed player)) < (fst platform + platformWidth /2)), 
  ((playerWidth player + widthOfPlayer + dt * (playerSpeed player)) > (fst platform - platformWidth /2))])
   where heigthOfPlayer = 1200 * 0.03
         widthOfPlayer = 800 * 0.03

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
   playerHeight = playerHeight player + dt * speed
}

keepPlayer :: Float -> Player-> Player
keepPlayer dt player = keepPlayerOnScreen dt (keepPlayerOnPlatform dt player)

movePlayer :: Float -> Player -> Player
movePlayer dt player = player {
  playerFallingSpeed = (playerFallingSpeed player) + dt * gravity,
  playerHeight = (playerHeight player) + dt * ((playerFallingSpeed player) + dt * (gravity / 2))
}


-- | Обновить платформы игровой вселенной
updatePlatforms :: Float -> [Platform] -> [Platform]
updatePlatforms _ [] = []
updatePlatforms dt ((width, offset) : platforms)
  | dy > pos  = updatePlatforms dt' platforms
  | otherwise = (width, offset - dy) : platforms
  where
    pos = screenUp - offset + platformHeight
    dy  =  dt * speed
    dt' = dt -  offset / speed
    
updatePlayer :: Float -> Player -> Player
updatePlayer dt player = (keepPlayerOnScreen dt (movePlayer dt player))