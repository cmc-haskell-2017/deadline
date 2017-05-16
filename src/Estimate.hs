module Estimate where

import Types
import Collides
import TypesAI

-- |
getEstimate :: Float -> GameTree Universe -> GameTree Estimate
getEstimate dt (Leaf t) = Leaf (estimate dt t t)
getEstimate dt (Node trees) = Node (map g trees)
  where g (move, tree) = (move, gtmap (\a -> (estimate dt (moveUniverse move) a)) tree)


-- | Оценить вселенную.
estimate :: Float -> Universe -> Universe -> Estimate
estimate dt u ul
  | not (universePlay u) || not (universePlay ul) = 0.0
  | otherwise = estimatePlatforms * (estimateFloor (universeRobot u)) * (estimateHight (universeRobot u)) * estimateBullets * (estimateFloor (universeRobot ul)) * (estimateHight (universeRobot ul)) * estimatePlatforms2
  where
    estimatePlatforms = (estimatePlatformAndBullet (maxWayPlatforms dt) (universeRobot u) (universePlatforms u))
    estimatePlatforms2 = (estimatePlatformAndBullet (maxWayPlatforms dt) (universeRobot ul) (universePlatforms ul))
    estimateBullets = (estimatePlatformAndBullet maxWayBullets (universeRobot u) (cannonBullets (universeCannon u)))

-- |
estimatePlatformAndBullet :: (Player -> [a] -> Float) -> Player -> [a] -> Float
estimatePlatformAndBullet f player list = (maxS - (f player list))/maxS

-- | 
maxWayPlatforms :: Float -> Player -> [Platform] -> Float
maxWayPlatforms dt player platforms
  | m == [] = 0.0
  | otherwise = maxS - (maximum m)
  where m = (mapPlatforms dt player platforms)

-- |
mapPlatforms :: Float -> Player -> [Platform] -> [Float]
mapPlatforms dt player ((width, offset, time) : ss) 
  | offset < screenDown + 60 = []
  | (playerHeight player - heightOfPlayer/2 < offset + platformHeight/2) = mapPlatforms dt player ss
--  | heightP > ((playerFallingSpeed player) * (widthP/bumpSpeed) + gravity * (widthP/bumpSpeed) * (widthP/bumpSpeed)/2) = (mapPlatforms dt player ss)
  | otherwise = (maxS - sqrt (heightP * heightP + widthP * widthP)) : (mapPlatforms dt player ss)
  where 
    heightP = if (fst (collides dt player (width, offset, time))) 
              then 0.0
              else abs (playerHeight player - heightOfPlayer/2 - offset - platformHeight)
    widthP = if (playerWidth player - widthOfPlayer/2 < offset + platformWidth/2) && (playerWidth player + widthOfPlayer/2 > offset - platformWidth/2)
             then 0.0
             else minimum [abs (playerWidth player - widthOfPlayer/2 - offset - platformWidth/2), abs (playerWidth player + widthOfPlayer/2 - offset + platformWidth/2)]

-- |
maxWayBullets :: Player -> [Bullet] -> Float
maxWayBullets _ [] = 1.0
maxWayBullets player bullets = (maximum (map g bullets))
  where 
    g bullet = if (playerWidth player - widthOfPlayer/2 < bulletWidth bullet + bulletsWidth) && (playerWidth player + widthOfPlayer/2 > bulletWidth bullet - bulletsWidth) 
               then 1.0 
               else 0.0

-- |
estimateHight :: Player -> Float
estimateHight player 
  | dist > 1 = 1
  | otherwise = dist
  where dist = (screenUp - 60 - playerHeight player - heightOfPlayer/2)/(screenUp) 

-- | 
estimateFloor :: Player -> Float
estimateFloor player 
  | dist > 1 = 1
  | otherwise = dist
  where dist = (playerHeight player - heightOfPlayer/2 - screenDown - 60)/(screenUp) 

