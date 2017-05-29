module Update where

import Types
import Draw
import Init
import Cannon
import Collides


-- | Конец игры?
isGameOver :: Float -> Player -> Universe -> Bool
isGameOver dt player u = playerBelowFloor || playerBelowRoof || (playerKilled dt player u)
  where
    playerBelowRoof = playerHeight player >  screenUp - 30
    playerBelowFloor = playerHeight player < screenDown + 30

-- | Обновление местоположения игрока по горизонтали.
keepPlayerOnScreen :: Float -> Player -> Player 
keepPlayerOnScreen dt player = player {
  playerWidth = (max (min w (playerWidth player) + dt * (playerSpeed player)) wm)
} 
  where
    w = fromIntegral screenWidth/2
    wm = -(fromIntegral screenWidth/2)

-- | Вспомогательная функция для игрока на платформе.
keepPlayerOnPlatform :: Float -> Player -> Player
keepPlayerOnPlatform dt player = player {
   playerFallingSpeed = speed,
   playerHeight = playerHeight player + dt * speed,
   playerIsOnPlatform = True
}

-- | Вспомогательная функция для игрока, столкнувшегося с платформой.
holdPlayerOnPlatform :: Float -> Player -> Player
holdPlayerOnPlatform dt player = player {
   playerSpeed = 0,
   playerFallingSpeed = (playerFallingSpeed player) + dt * gravity,
   playerHeight = (playerHeight player) + dt * ((playerFallingSpeed player) + dt * (gravity / 2)),
   playerIsOnPlatform = False
}

-- | Обновление игрока на платформе.
keepPlayer :: Float -> Player -> Player
keepPlayer dt player = keepPlayerOnScreen dt (keepPlayerOnPlatform dt player)

-- | Обновление игрока, столкнувшегося с платформой.
holdPlayer :: Float -> Player -> Player
holdPlayer dt player = keepPlayerOnScreen dt (holdPlayerOnPlatform dt player)

-- | Обновление скорости и расположения по вертикали игрока.
movePlayer :: Float -> Player -> Player
movePlayer dt player = player {
  playerFallingSpeed = (playerFallingSpeed player) + dt * gravity,
  playerHeight = (playerHeight player) + dt * ((playerFallingSpeed player) + dt * (gravity / 2)),
  playerIsOnPlatform = False
}

-- | Обновить платформы игровой вселенной.
updatePlatforms :: Float -> [Platform] -> Universe -> [Platform]
updatePlatforms _ [] _  = []
updatePlatforms dt ((width, offset, time) : platforms) u 
  | screenUp < offset = updatePlatforms dt platforms u
  | isKilledPlatform dt u (width, offset, time) = updatePlatforms dt platforms u
  | time - dt < 0 = updatePlatforms dt platforms u
  | (collidesHelper (playerSquare player dt) (platformSquare (width, offset, time) dt)) &&  livePlayer = (width, offset + dy, time - dt) : (updatePlatforms dt platforms u)
  | otherwise = (width, offset + dy, time) : (updatePlatforms dt platforms u)
  where
        player = universePlayer u
        dy  = dt * speed

-- | Обновление состояния игрока.
updatePlayer :: Float -> Player -> Player
updatePlayer dt player = (keepPlayerOnScreen dt (movePlayer dt player))

updateBackground :: Float -> Background -> Background
updateBackground dt bg
  | (bgHeight1 bg) >= 7900 = bg {
  bgHeight1 = -bgHeight,
  bgHeight2 = (bgHeight2 bg) + dt * (bgSpeed bg)
}
  | (bgHeight2 bg) >= 7900 = bg {
  bgHeight1 = (bgHeight1 bg) + dt * (bgSpeed bg),
  bgHeight2 = -bgHeight
}
  | otherwise = bg {
  bgHeight1 = (bgHeight1 bg) + dt * (bgSpeed bg),
  bgHeight2 = (bgHeight2 bg) + dt * (bgSpeed bg)
}

