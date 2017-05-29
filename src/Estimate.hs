module Estimate where

import Types
import Collides
import TypesAI

-- | Условие схода с платформы.
estimateScreenUp :: Float -> Universe -> Bool
estimateScreenUp dt u = (fst (isWithPlatform dt player u)) && (screenUp - speed - playerHeight player - heightOfPlayer < platformWidth + widthOfPlayer)
 where
   player = universeRobot u

-- | Условие ухода от пули.
estimateBullet :: Float -> Universe -> Bool
estimateBullet dt u = (fstBullet sbul) && (playerWidth player + widthOfPlayer/2 > bulletWidth bullet - bulletsWidth/2) && (playerWidth player - widthOfPlayer/2 < bulletWidth bullet + bulletsWidth/2)
  where 
    player = universeRobot u
    sbul = cannonBullets (universeCannon u)
    bullet = mapBullets dt (universeRobot u) (cannonBullets (universeCannon u))

-- | Определить есть ли пули.
fstBullet :: [Bullet] -> Bool
fstBullet [] = False
fstBullet _ = True

-- | Выбрать подходящую платформу.
bestPlatform :: Player -> [(Float, Platform)] -> Platform
bestPlatform player [] = (0.0, 0.0, -1.0)
bestPlatform player ((x, xs) : []) = xs
bestPlatform player ((long1, (width1, offset1, life1)) : (long2, (width2, ofsset2, life2)) : ss)
  | long1 > long2 = bestPlatform player ((long1, (width1, offset1, life1)) : ss)
  | otherwise = bestPlatform player ((long2, (width2, ofsset2, life2)) : ss)

-- | Выбрать подходящую платформу, за исключением передаваемой.
bestPlatformWithout :: Float -> Player -> Platform -> [(Float, Platform)] -> Platform
bestPlatformWithout _ _ _ [] = (0.0, 0.0, -1.0)
bestPlatformWithout dt player (width1, offset1, life1) ((long, (width2, offset2, life2)) : []) 
  | (width1 == width2) && (offset1 + dt * speed == offset2) = (0.0, 0.0, -1.0)
  | otherwise = (width2, offset2, life2)
bestPlatformWithout dt player (width, offset, life) ((long1, (width1, offset1, life1)) : (long2, (width2, offset2, life2)) : ss)
  | (width == width1) && (offset + dt * speed == offset1) = bestPlatformWithout dt player (width, offset, life) ((long2, (width2, offset2, life2)) : ss)
  | (width == width2) && (offset + dt * speed == offset2) = bestPlatformWithout dt player (width, offset, life) ((long1, (width1, offset1, life1)) : ss)
  | long1 > long2 = bestPlatformWithout dt player (width, offset, life) ((long1, (width1, offset1, life1)) : ss)
  | otherwise = bestPlatformWithout dt player (width, offset, life) ((long2, (width2, offset2, life2)) : ss)

-- | Отфильтровать список платформ. Выбрать подходящие и вычислить их оценки. !!!
mapPlatforms :: Float -> Player -> [Platform] -> [(Float, Platform)]
mapPlatforms dt player ((width, offset, time) : ss) 
  | offset < screenDown = []
 -- | screenUp - speed/2 - offset < platformWidth + widthOfPlayer = mapPlatforms dt player ss 
  | (playerHeight player - heightOfPlayer/2 < offset + platformHeight/2) = mapPlatforms dt player ss
  | heightP < abs ((playerFallingSpeed player) * (widthP/speed) + gravity * (widthP/speed) * (widthP/speed) /2) + widthP = (mapPlatforms dt player ss)
  | otherwise = ((sqrt (heightP * heightP)), (width, offset, time)) : (mapPlatforms dt player ss)
  where 
    heightP = minHeight dt player (width, offset, time)
    widthP = minWidth player (width, offset, time)

-- | Расстояние от платформы до ИИ по вертикали.
minHeight :: Float -> Player -> Platform -> Float
minHeight dt player (width, offset, time)
  | fst (collides dt player (width, offset, time)) = 0.0
  | otherwise = abs (playerHeight player - heightOfPlayer/2 - offset - platformHeight)

-- | Расстояние от платформы до ИИ по горизонтали.
minWidth :: Player -> Platform -> Float
minWidth player (width, offset, time)
  | (playerWidth player - widthOfPlayer/2 < width + platformWidth/2) && (playerWidth player + widthOfPlayer/2 > width - platformWidth/2) = 0.0
  | otherwise = minimum [abs (playerWidth player - widthOfPlayer/2 - width - platformWidth/2), abs (playerWidth player + widthOfPlayer/2 - width + platformWidth/2)]

-- | Определить платформу, на которой стоит в данный момент ИИ.
thisPlatform :: Float -> Player -> [Platform] -> Platform
thisPlatform _ _ [] = (0.0, 0.0, -1.0)
thisPlatform dt player (platform : ss)
  | fst (collides dt player platform) = platform
  | otherwise = thisPlatform dt player ss

-- | Определить пулю, которая может в данный момент убить ИИ.
mapBullets :: Float -> Player -> [Bullet] -> Bullet
mapBullets _ _ (bullet : []) = bullet
mapBullets dt player (bullet : ss) 
  | (playerWidth player + widthOfPlayer/2 > bulletWidth bullet - bulletsWidth/2) && (playerWidth player - widthOfPlayer/2 < bulletWidth bullet + bulletsWidth/2) = bullet
  | otherwise = mapBullets dt player ss
