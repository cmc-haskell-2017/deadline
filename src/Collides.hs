module Collides where

import Types
import Draw
import Init


-- | Проверка на столкновение.
isWithPlatform :: Float -> Player -> Universe -> (Bool, Bool)
isWithPlatform dt player u = playerWithPlatform
  where
    playerWithPlatform = (collision dt player (universePlatforms u))

-- | 
isKilledPlatform :: Float -> Universe -> Platform -> Bool
isKilledPlatform dt u platform = or (map (\bullet -> killPlatform dt bullet platform) (cannonBullets (universeCannon u)))

-- |
killPlatforms :: Float -> [Platform] -> Bullet -> Bool
killPlatforms _ [] _ = False
killPlatforms dt ((width, offset, time) : platforms) bullet 
  | screenDown > offset = False
  | otherwise = or [(killPlatform dt bullet (width, offset, time)), (killPlatforms dt platforms bullet)]

-- |
killPlatform :: Float -> Bullet -> Platform -> Bool
killPlatform dt bullet platform = collidesHelper (platformSquare platform dt) (bulletSquare bullet dt)

-- | Сталкивается ли игрок с любыми из
-- бесконечного списка ворот?
collision :: Float -> Player -> [Platform] -> (Bool, Bool)
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
collides :: Float -> Player -> Platform -> (Bool, Bool)
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

-- | Пямоугольник игрока.
playerSquare :: Player -> Float -> Square
playerSquare player dt = Square {
          xCoordinateLeft = (playerWidth player - widthOfPlayer),
          yCoordinateRight = (playerHeight player - heightOfPlayer), 
          xCoordinateRight = (playerWidth player + widthOfPlayer),
          yCoordinateLeft = (playerHeight player + heightOfPlayer), 
          xSpeed = dt * (playerSpeed player),
          ySpeed = dt * (playerFallingSpeed player)
}

-- | Прямоугольник платформы.
platformSquare :: Platform -> Float -> Square
platformSquare (width, offset, time) dt = Square {
          xCoordinateLeft = width - platformWidth /2,
          yCoordinateRight = offset, 
          xCoordinateRight = width + platformWidth /2,
          yCoordinateLeft = offset + platformHeight, 
          xSpeed = 0,
          ySpeed = speed * dt
}


-- |
bulletSquare :: Bullet -> Float -> Square
bulletSquare bullet dt = Square {
          xCoordinateLeft = (bulletWidth bullet) - bulletsWidth /2,
          yCoordinateRight = bulletHeight bullet - bulletsHeight/2, 
          xCoordinateRight = (bulletWidth bullet) + bulletsWidth /2,
          yCoordinateLeft = (bulletHeight bullet) + bulletsHeight/2, 
          xSpeed = 0,
          ySpeed = bulletSpeed * dt
}
    
