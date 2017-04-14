module Update where

import Types
import Draw
import Init

--1) чувачок мог подпрыгивать, только с платформы
--2) отрисовка текста - сделать функцию которая жирно отрисовывает текст (сколько раз и с каким сдвигом)
--3) сделать чтобы фон двигался (скорость меньше чем скорость всего остального)
--4) уничтожение платформы со временем


-- | Обновить состояние игровой вселенной.
updateUniverse :: Float -> Universe -> Universe
updateUniverse dt u
  | isGameOver u = u { universeGameOver = Just initGameOver }
  | fst (isWithPlatform dt u) = (upUniverse dt u) {universePlayer = keepPlayer dt (universePlayer u)}
  | snd (isWithPlatform dt u) = (upUniverse dt u) {universePlayer = holdPlayer dt (universePlayer u)}
  | otherwise = (upUniverse dt u) {universePlayer = updatePlayer dt (universePlayer u)}
 
-- | Обновление вселенной.
upUniverse:: Float -> Universe -> Universe 
upUniverse dt u = u { universePlatforms  = updatePlatforms  dt (universePlatforms  u)
      , universeScore  = (universeScore u) + dt
      , universeBackground = updateBackground dt (universeBackground u)
      }

-- | Проверка на столкновение.
isWithPlatform :: Float -> Universe -> (Bool, Bool)
isWithPlatform dt u = playerWithPlatform
  where
    playerWithPlatform = (collision dt (universePlayer u) (universePlatforms u))

-- | Конец игры?
isGameOver :: Universe -> Bool
isGameOver u = playerBelowFloor || playerBelowRoof
  where
    playerBelowRoof = playerHeight (universePlayer u) >  screenUp - 30
    playerBelowFloor = playerHeight (universePlayer u) < screenDown + 30

-- | Сталкивается ли игрок с любыми из
-- бесконечного списка ворот?
collision :: Float -> Player -> [Platform] -> (Bool, Bool)
collision _ _ [] = (False, False)
collision dt player platforms = tupleOr (map (collides dt player) (takeWhile onScreen (absolutePlatforms platforms)))
  where
    onScreen (_, offset) = offset - platformHeight > screenDown

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
collides :: Float -> Player -> Platform -> (Bool, Bool)
collides dt player platform = ((collidesHelper playerSquare platformSquare), 
  or [(collidesHelper (rotateLeft playerSquare) (rotateLeft platformSquare)), 
  (collidesHelper (rotateRight playerSquare) (rotateRight platformSquare))])
  where playerSquare = Square {
          xCoordinateLeft = (playerWidth player - widthOfPlayer),
          yCoordinateRight = (playerHeight player - heigthOfPlayer), --  + dt * (playerFallingSpeed player)),
          xCoordinateRight = (playerWidth player + widthOfPlayer),
          yCoordinateLeft = (playerHeight player + heigthOfPlayer), -- + dt * (playerFallingSpeed player)),
          xSpeed = dt * (playerSpeed player),
          ySpeed = dt * (playerFallingSpeed player)
        }
        platformSquare = Square {
          xCoordinateLeft = (fst platform - platformWidth /2),
          yCoordinateRight = (snd platform), -- + dt * speed),
          xCoordinateRight = (fst platform + platformWidth /2),
          yCoordinateLeft = (snd platform + platformHeight), -- + dt * speed)
          xSpeed = 0,
          ySpeed = speed * dt
        }
        heigthOfPlayer = 1200 * 0.03
        widthOfPlayer = 800 * 0.03

-- | Проверка на столкновение.
collidesHelper :: Square -> Square -> Bool
collidesHelper player platform = 
  (and [(yCoordinateRight player > yCoordinateRight platform),
  (yCoordinateRight player + ySpeed player < yCoordinateLeft platform + ySpeed platform), 
  (yCoordinateRight player + platformHeight/4 > yCoordinateLeft platform), 
  (xCoordinateLeft player < xCoordinateRight platform), 
  (xCoordinateRight player > xCoordinateLeft platform)])

-- | Обновление местоположения игрока по горизонтали.
keepPlayerOnScreen :: Float -> Player -> Player 
keepPlayerOnScreen dt player = player {
  playerWidth = (max (min w (playerWidth player) + dt * (playerSpeed player)) wm)
} 
  where
    w = 200
    wm = -200 

-- | Вспомогательная функция для игрока на платформе.
keepPlayerOnPlatform :: Float -> Player -> Player
keepPlayerOnPlatform dt player = player {
   playerFallingSpeed = speed,
   playerHeight = playerHeight player + dt * speed
}

-- | Вспомогательная функция для игрока, столкнувшегося с платформой.
holdPlayerOnPlatform :: Float -> Player -> Player
holdPlayerOnPlatform dt player = player {
   playerSpeed = 0,
   playerFallingSpeed = (playerFallingSpeed player) + dt * gravity,
   playerHeight = (playerHeight player) + dt * ((playerFallingSpeed player) + dt * (gravity / 2))
}

-- | Обновление игрока на платформе.
keepPlayer :: Float -> Player-> Player
keepPlayer dt player = keepPlayerOnScreen dt (keepPlayerOnPlatform dt player)

-- | Обновление игрока, столкнувшегося с платформой.
holdPlayer :: Float -> Player-> Player
holdPlayer dt player = keepPlayerOnScreen dt (holdPlayerOnPlatform dt player)

-- | Обновление скорости и расположения по вертикали игрока.
movePlayer :: Float -> Player -> Player
movePlayer dt player = player {
  playerFallingSpeed = (playerFallingSpeed player) + dt * gravity,
  playerHeight = (playerHeight player) + dt * ((playerFallingSpeed player) + dt * (gravity / 2))
}

-- | Обновить платформы игровой вселенной.
updatePlatforms :: Float -> [Platform] -> [Platform]
updatePlatforms _ [] = []
updatePlatforms dt ((width, offset) : platforms)
  | dy > pos  = updatePlatforms dt' platforms
  | otherwise = (width, offset - dy) : platforms
  where
    pos = screenUp - offset + platformHeight
    dy  =  dt * speed
    dt' = dt -  offset / speed
    
-- | Обновление состояния игрока.
updatePlayer :: Float -> Player -> Player
updatePlayer dt player = (keepPlayerOnScreen dt (movePlayer dt player))

updateBackground :: Float -> Background -> Background
updateBackground dt bg
  | (bgHeight1 bg) > 700 = bg {
  bgHeight1 = (bgHeight2 bg) - 690,
  bgHeight2 = (bgHeight2 bg) + dt * (bgSpeed bg)
}
  | (bgHeight2 bg) > 700 = bg {
  bgHeight1 = (bgHeight1 bg) + dt * (bgSpeed bg),
  bgHeight2 = (bgHeight1 bg) - 690
}
  | otherwise = bg {
  bgHeight1 = (bgHeight1 bg) + dt * (bgSpeed bg),
  bgHeight2 = (bgHeight2 bg) + dt * (bgSpeed bg)
}




