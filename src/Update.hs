module Update where

import Types
import Draw
import Init

-- | Обновить состояние игровой вселенной.
updateUniverse :: Time -> Universe -> Universe
updateUniverse dt u
  | isGameOver u = u { universeGameOver = Just initGameOver }
  | fst (isWithPlatform dt u) = (upUniverse dt u) {universePlayer = keepPlayer dt (universePlayer u)}
  | snd (isWithPlatform dt u) = (upUniverse dt u) {universePlayer = holdPlayer dt (universePlayer u)}
  | isWithBonus dt u (universeBonusBottle u) = (upUniverse dt u) {universePlayer = updatePlayer dt (giveBottle (universePlayer u))}
  | isWithBonus dt u (universeBonusCoffee u) = (upUniverse dt u) {universePlayer = updatePlayer dt (giveCoffee (universePlayer u))}
  | isWithBonus dt u (universeBonusRedBull u) = (upUniverse dt u) {universePlayer = updatePlayer dt (giveRedBull (universePlayer u))}
  | isWithBonus dt u (universeBonusSpeed u) = (upUniverse dt u) {universePlayer = updatePlayer dt (giveSpeed (universePlayer u))}
  | isWithBonus dt u (universeBonusStar u) = (upUniverse dt u) {universeScore = (universeScore u) + starBonusScore}
  | otherwise = (upUniverse dt u) {universePlayer = updatePlayer dt (universePlayer u)}
-- | Обновление вселенной.
upUniverse:: Time -> Universe -> Universe 
upUniverse dt u = u { universePlatforms  = updatePlatforms  dt (universePlatforms  u)(universePlayer u)
                    , universeBonusBottle = updateBonus dt (universeBonusBottle u)(universePlayer u)
                    , universeBonusStar = updateBonus dt (universeBonusStar u)(universePlayer u)
                    , universeBonusSpeed = updateBonus dt (universeBonusSpeed u)(universePlayer u)
                    , universeBonusRedBull = updateBonus dt (universeBonusRedBull u)(universePlayer u)
                    , universeBonusCoffee = updateBonus dt (universeBonusCoffee u)(universePlayer u)
                    , universeScore  = (universeScore u) + dt
                    , universeBackground = updateBackground dt (universeBackground u)
                    }

-- -------------------------------------------------------------------------

-- | Есть ли True в паре (Bool, Bool)
oinb :: (Bool, Bool) -> Bool
oinb (False, False) = False
oinb (_, _) = True

giveBottle :: Player -> Player
giveBottle player = player {bonusBottleInt = (True, timeOfBonus)}

giveCoffee :: Player -> Player
giveCoffee player = player {bonusCoffeeInt = (True, timeOfBonus)}

giveRedBull :: Player -> Player
giveRedBull player = player {bonusRedBullInt = (True, timeOfBonus)}

giveSpeed :: Player -> Player
giveSpeed player = player {bonusSpeedInt = (True, timeOfBonus)}

isWithBonus :: Time -> Universe -> [Bonus] -> Bool
isWithBonus dt u b = oinb (collisionBonuses dt (universePlayer u) b)

collisionBonuses :: Time -> Player -> [Bonus] -> (Bool, Bool)
collisionBonuses _ _ [] = (False, False)
collisionBonuses dt player bonus = tupleOr (map (collidesBonus dt player) (takeWhile onScreen bonus))
  where
    onScreen (_, offset) = offset - bonusSize > screenDown

-- |  Становится ли игрок на платформу?
collidesBonus :: Time -> Player -> Bonus -> (Bool, Bool)
collidesBonus dt player (width, offset) = ((collidesBonusHelper (playerSquare player dt) (bonusSquare (width, offset) dt)), 
  or [(collidesBonusHelper (rotateLeft (playerSquare player dt)) (rotateLeft (bonusSquare (width, offset) dt))), 
  (collidesBonusHelper (rotateRight (playerSquare player dt)) (rotateRight (bonusSquare (width, offset) dt)))])

-- | Прямоугольник бонуса.
bonusSquare :: Bonus -> Time -> Square
bonusSquare (width, offset) dt = Square {
          xCoordinateLeft = width - bonusSize /2,
          yCoordinateRight = offset, 
          xCoordinateRight = width + bonusSize /2,
          yCoordinateLeft = offset + bonusSize, 
          xSpeed = 0,
          ySpeed = speedOfBonus * dt
}

-- | Проверка на столкновение.
collidesBonusHelper :: Square -> Square -> Bool
collidesBonusHelper player bonus = 
  (and [(yCoordinateRight player > yCoordinateRight bonus),
  (yCoordinateRight player + ySpeed player < yCoordinateLeft bonus + ySpeed bonus), 
  (yCoordinateRight player + platformHeight/4 > yCoordinateLeft bonus), 
  (xCoordinateLeft player < xCoordinateRight bonus), 
  (xCoordinateRight player > xCoordinateLeft bonus)])

-- | Обновить бутылки игровой вселенной.
updateBonus :: Time -> [Bonus] -> Player -> [Bonus]
updateBonus _ [] _ = []
updateBonus dt ((width, offset) : bonus) player
  | screenUp < offset = updateBonus dt bonus player
  | (oinb (collidesBonus dt player (width, offset))) = (updateBonus dt bonus player)
  | otherwise = (width, offset + dy) : (updateBonus dt bonus player)
  where
        dy  = dt * speedOfBonus

updatePlayerBonusTime :: Time -> Player -> Player
updatePlayerBonusTime dt player = player {
    bonusBottleInt = (upd (bonusBottleInt player) dt),
    bonusSpeedInt = (upd (bonusSpeedInt player) dt),
    bonusRedBullInt = (upd (bonusRedBullInt player) dt),
    bonusCoffeeInt = (upd (bonusCoffeeInt player) dt)
}

upd :: PlayerBonus -> Time -> PlayerBonus
upd (False, _) _ = (False, 0)
upd (True, time) dt 
    | time - dt < 0 = (False, 0)
    | otherwise = (True, time - dt)

-- ----------------------------------------------------------------------------
   
-- | Проверка на столкновение.
isWithPlatform :: Time -> Universe -> (Bool, Bool)
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
collision :: Time -> Player -> [Platform] -> (Bool, Bool)
collision _ _ [] = (False, False)
collision dt player platforms = tupleOr (map (collides dt player) (takeWhile onScreen platforms))
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
collides :: Time -> Player -> Platform -> (Bool, Bool)
collides dt player (width, offset, life) = ((collidesHelper (playerSquare player dt) (platformSquare (width, offset, life) dt)), 
  or [(collidesHelper (rotateLeft (playerSquare player dt)) (rotateLeft (platformSquare (width, offset, life) dt))), 
  (collidesHelper (rotateRight (playerSquare player dt)) (rotateRight (platformSquare (width, offset, life) dt)))])

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

-- | Обновление состояния игрока.
updatePlayer :: Time -> Player -> Player
updatePlayer dt player = (keepPlayerOnScreen dt (updatePlayerBonusTime dt (movePlayer dt player)))

-- | Вспомогательная функция для игрока на платформе.
keepPlayerOnPlatform :: Time -> Player -> Player
keepPlayerOnPlatform dt player = player {
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
keepPlayer :: Time -> Player-> Player
keepPlayer dt player = keepPlayerOnScreen dt (keepPlayerOnPlatform dt player)

-- | Обновление игрока, столкнувшегося с платформой.
holdPlayer :: Time -> Player-> Player
holdPlayer dt player = keepPlayerOnScreen dt (holdPlayerOnPlatform dt player)

-- | Обновление скорости и расположения по вертикали игрока.
movePlayer :: Time -> Player -> Player
movePlayer dt player
  | fst (bonusRedBullInt player) = player {
    playerIsOnPlatform = False,
    playerHeight = (playerHeight player) + dt * (playerFallingSpeed player)
}
  | fst (bonusBottleInt player) = player {
    playerFallingSpeed = 0,
    playerIsOnPlatform = False,
    playerHeight = (playerHeight player) + dt * (gravity / 25)
}
  | otherwise = player {
    playerFallingSpeed = (playerFallingSpeed player) + dt * gravity,
    playerIsOnPlatform = False,
    playerHeight = (playerHeight player) + dt * ((playerFallingSpeed player) + dt * (gravity / 2))
}

-- | Обновить платформы игровой вселенной.
updatePlatforms :: Time -> [Platform] -> Player -> [Platform]
updatePlatforms _ [] _ = []
updatePlatforms dt ((width, offset, time) : platforms) player
  | screenUp < offset = updatePlatforms dt platforms player
  | time - dt < 0 = updatePlatforms dt platforms player
  | collidesHelper (playerSquare player dt) (platformSquare (width, offset, time) dt) = (width, offset + dy, time - dt) : (updatePlatforms dt platforms player)
  | otherwise = (width, offset + dy, time) : (updatePlatforms dt platforms player)
  where
        dy = speedBonusCheck dt player

speedBonusCheck :: Time -> Player -> Offset
speedBonusCheck dt player 
        | fst (bonusSpeedInt player) = dt * speed / 2
        | otherwise = dt * speed

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
platformSquare :: Platform -> Time -> Square
platformSquare (width, offset, _) dt = Square {
          xCoordinateLeft = width - platformWidth /2,
          yCoordinateRight = offset, 
          xCoordinateRight = width + platformWidth /2,
          yCoordinateLeft = offset + platformHeight, 
          xSpeed = 0,
          ySpeed = speed * dt
}

updateBackground :: Time -> Background -> Background
updateBackground dt bg
  | (bgHeight1 bg) >= (bgSize bg) = bg {
  bgHeight1 = -(bgSize bg),
  bgHeight2 = (bgHeight2 bg) + dt * (bgSpeed bg)
}
  | (bgHeight2 bg) >= (bgSize bg) = bg {
  bgHeight1 = (bgHeight1 bg) + dt * (bgSpeed bg),
  bgHeight2 = -(bgSize bg)
}
  | otherwise = bg {
  bgHeight1 = (bgHeight1 bg) + dt * (bgSpeed bg),
  bgHeight2 = (bgHeight2 bg) + dt * (bgSpeed bg)
}