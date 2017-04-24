module Update where

import Types
import Draw
import Init

-- | Обновить состояние игровой вселенной.
updateUniverse :: Time -> Universe -> Universe
updateUniverse dt u
  | isGameOver u = u { universeGameOver = Just initGameOver }
  | fst (isWithPlatform dt u) = (upUniverse dt u) {universePlayer = keepPlayer (speed u) dt (universePlayer u)}
  | snd (isWithPlatform dt u) = (upUniverse dt u) {universePlayer = holdPlayer dt (universePlayer u)}
  | otherwise = (upUniverse dt u) {universePlayer = updatePlayer dt (universePlayer u)}
 
-- | Обновление вселенной.
upUniverse:: Time -> Universe -> Universe 
upUniverse dt u = u { universePlatforms  = updatePlatforms (speed u)  dt (universePlatforms  u)(universePlayer u)
      , universeScore  = (universeScore u) + dt
      , universeBackground = updateBackground (speed u) dt (universeBackground u)
      , speed = (speed u) + dt*7
      }

-- | Проверка на столкновение.
isWithPlatform :: Time -> Universe -> (Bool, Bool)
isWithPlatform dt u = playerWithPlatform
  where
    playerWithPlatform = (collision (speed u) dt (universePlayer u) (universePlatforms u))

-- | Конец игры?
isGameOver :: Universe -> Bool
isGameOver u = playerBelowFloor || playerBelowRoof
  where
    playerBelowRoof = playerHeight (universePlayer u) >  screenUp - 30
    playerBelowFloor = playerHeight (universePlayer u) < screenDown + 30

-- | Сталкивается ли игрок с любыми из
-- бесконечного списка ворот?
collision :: Speed -> Time -> Player -> [Platform] -> (Bool, Bool)
collision _ _ _ [] = (False, False)
collision speed dt player platforms = tupleOr (map (collides speed dt player) (takeWhile onScreen platforms))
  where
    onScreen (_, offset, _) = offset - platformHeight > screenDown

-- | Проверка на столкновение свреху.
tupleOrFirst :: [(Bool, Bool)] -> Bool
tupleOrFirst [] = False
tupleOrFirst list = or (map fst list)

-- | Проверка на столкновение сбоку.
tupleOrSecond :: [(Bool, Bool)] -> Bool
tupleOrSecond [] = False
tupleOrSecond list = or (map snd list)

-- | Свести список кортежей к одному кортежу.
tupleOr :: [(Bool, Bool)] -> (Bool, Bool)
tupleOr [] = (False, False)
tupleOr list = ((tupleOrFirst list), (tupleOrSecond list))

-- | Перевернуть прямоугольник, для проверки на столкновение слева.
rotateLeft :: Square -> Square
rotateLeft square = Square {
                  yCoordinateRight = -(xCoordinateRight square),
                  yCoordinateLeft = -(xCoordinateLeft square),
                  xCoordinateLeft = -(yCoordinateLeft square),
                  xCoordinateRight = -(yCoordinateRight square),
                  xSpeed = ySpeed square,
                  ySpeed = - (xSpeed square)
                }

-- | Перевернуть прямоугольник, для проверки на столкновение справа.
rotateRight :: Square -> Square
rotateRight square = Square {
                  yCoordinateRight = (xCoordinateLeft square),
                  yCoordinateLeft = (xCoordinateRight square),
                  xCoordinateLeft = (yCoordinateRight square),
                  xCoordinateRight = (yCoordinateLeft square),
                  xSpeed = ySpeed square,
                  ySpeed = xSpeed square
                }

-- |  Становится ли игрок на платформу?
collides :: Speed -> Time -> Player -> Platform -> (Bool, Bool)
collides speed dt player (width, offset, life) = ((collidesHelper (playerSquare player dt) (platformSquare speed (width, offset, life) dt)), 
  or [(collidesHelper (rotateLeft (playerSquare player dt)) (rotateLeft (platformSquare speed (width, offset, life) dt))), 
  (collidesHelper (rotateRight (playerSquare player dt)) (rotateRight (platformSquare speed (width, offset, life) dt)))])

-- | Проверка на столкновение.
collidesHelper :: Square -> Square -> Bool
collidesHelper player platform = 
  (and [(yCoordinateRight player > yCoordinateRight platform),
  (yCoordinateRight player + ySpeed player < yCoordinateLeft platform + ySpeed platform), 
  (yCoordinateRight player + platformHeight/4 > yCoordinateLeft platform), 
  (xCoordinateLeft player < xCoordinateRight platform), 
  (xCoordinateRight player > xCoordinateLeft platform)])

-- | Обновление местоположения игрока по горизонтали.
keepPlayerOnScreen :: Time -> Player -> Player 
keepPlayerOnScreen dt player = player {
  playerWidth = (max (min w (playerWidth player) + dt * (playerSpeed player)) wm)
} 
  where
    w = 200
    wm = -200 

-- | Вспомогательная функция для игрока на платформе.
keepPlayerOnPlatform :: Speed -> Time -> Player -> Player
keepPlayerOnPlatform speed dt player = player {
   playerFallingSpeed = speed,
   playerIsOnPlatform = True,
   playerHeight = playerHeight player + dt * speed
}

-- | Вспомогательная функция для игрока, столкнувшегося с платформой.
holdPlayerOnPlatform :: Time -> Player -> Player
holdPlayerOnPlatform dt player = player {
   playerSpeed = 0,
   playerIsOnPlatform = False,
   playerFallingSpeed = (playerFallingSpeed player) + dt * gravity,
   playerHeight = (playerHeight player) + dt * ((playerFallingSpeed player) + dt * (gravity / 2))
}

-- | Обновление игрока на платформе.
keepPlayer :: Speed -> Time -> Player-> Player
keepPlayer speed dt player = keepPlayerOnScreen dt (keepPlayerOnPlatform speed dt player)

-- | Обновление игрока, столкнувшегося с платформой.
holdPlayer :: Time -> Player-> Player
holdPlayer dt player = keepPlayerOnScreen dt (holdPlayerOnPlatform dt player)

-- | Обновление скорости и расположения по вертикали игрока.
movePlayer :: Time -> Player -> Player
movePlayer dt player = player {
  playerFallingSpeed = (playerFallingSpeed player) + dt * (gravity + (coefSpeed player)),
  playerIsOnPlatform = False,
  playerHeight = (playerHeight player) + dt * ((playerFallingSpeed player) + dt * (gravity / 2)),
  coefSpeed = (coefSpeed player) - dt*100
}

-- | Обновить платформы игровой вселенной.
updatePlatforms :: Speed -> Time -> [Platform] -> Player -> [Platform]
updatePlatforms _ _ [] _ = []
updatePlatforms speed dt ((width, offset, time) : platforms) player
  | screenUp < offset = updatePlatforms speed dt platforms player
  | time - dt < 0 = updatePlatforms speed dt platforms player
  | collidesHelper (playerSquare player dt) (platformSquare speed (width, offset, time) dt) = (width, offset + dy, time - dt) : (updatePlatforms speed dt platforms player)
  | otherwise = (width, offset + dy, time) : (updatePlatforms speed dt platforms player)
  where
        dy  = dt * speed

-- | Пямоугольник игрока.
playerSquare :: Player -> Time -> Square
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

-- | Прямоугольник платформы.
platformSquare :: Speed -> Platform -> Time -> Square
platformSquare speed (width, offset, time) dt = Square {
          xCoordinateLeft = width - platformWidth /2,
          yCoordinateRight = offset, 
          xCoordinateRight = width + platformWidth /2,
          yCoordinateLeft = offset + platformHeight, 
          xSpeed = 0,
          ySpeed = speed * dt
}
    
-- | Обновление состояния игрока.
updatePlayer :: Time -> Player -> Player
updatePlayer dt player = (keepPlayerOnScreen dt (movePlayer dt player))

updateBackground :: Speed -> Time -> Background -> Background
updateBackground speed dt bg
  | (bgHeight1 bg) >= (bgSize bg) = bg {
  bgHeight1 = -(bgSize bg),
  bgHeight2 = (bgHeight2 bg) + dt * (bgSpeed bg),
  bgSpeed = speed / 3
}
  | (bgHeight2 bg) >= (bgSize bg) = bg {
  bgHeight1 = (bgHeight1 bg) + dt * (bgSpeed bg),
  bgHeight2 = -(bgSize bg),
  bgSpeed = speed / 3
}
  | otherwise = bg {
  bgHeight1 = (bgHeight1 bg) + dt * (bgSpeed bg),
  bgHeight2 = (bgHeight2 bg) + dt * (bgSpeed bg),
  bgSpeed = speed / 3
}