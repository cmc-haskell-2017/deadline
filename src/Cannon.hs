module Cannon where

import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Interface.Pure.Game
import System.Random
import Graphics.Gloss.Juicy
import Types
import Collides

-- | 
playerKilled :: Float -> Player -> Universe -> Bool
playerKilled dt player u = or (map (isKilled dt player) bullets)
  where 
    bullets = cannonBullets (universeCannon u)

-- |
isKilled :: Float -> Player -> Bullet -> Bool
isKilled dt player bullet = or[ (collidesHelper (playerSquare player dt) (bulletSquare bullet dt))
                              , (collidesHelper (rotateLeft (playerSquare player dt)) (rotateLeft (bulletSquare bullet dt)))
                              , (collidesHelper (rotateRight (playerSquare player dt)) (rotateRight (bulletSquare bullet dt)))
                              , (collidesHelper (bulletSquare bullet dt) (playerSquare player dt))]


-- | 
updateBullets :: Float -> Universe -> [Bullet] -> [Bullet]
updateBullets _ _ [] = []
updateBullets dt u (bullet : bullets) 
  | screenUp < (bulletHeight bullet) = updateBullets dt u bullets
  | killPlatforms dt (universePlatforms u) bullet = updateBullets dt u bullets
  | otherwise = (Bullet { bulletWidth = (bulletWidth bullet) , bulletHeight = (bulletHeight bullet) + dt*bulletSpeed}) : (updateBullets dt u bullets)

-- | 
updateCannon :: Float -> Universe -> Cannon
updateCannon dt u | under = cannonShot dt u
                  | left = cannonShift dt cannonNormSpeed u
                  | right = cannonShift dt (-cannonNormSpeed) u
  where 
    (under, left, right) = whereCannon u

-- |
whereCannon :: Universe -> (Bool, Bool, Bool)
whereCannon u 
  | (not livePlayer) || (abs (playerWidth (universePlayer u) - cannon) > abs (playerWidth (universeRobot u) - cannon)) = (
    or[(if livePlayer then (and 
      [ playerLeft < cannon
      , playerRight > cannon]) else False), and [ aiLeft < cannon
                                   , aiRight > cannon]],
    aiLeft > cannon,
    aiRight < cannon)
  |otherwise = (
    or[and 
      [ playerLeft < cannon
      , playerRight > cannon], and [ aiLeft < cannon
                                   , aiRight > cannon]],
    playerLeft > cannon,
    playerRight < cannon)
  where
    playerLeft = playerWidth (universePlayer u) - widthOfPlayer/2
    playerRight = playerWidth (universePlayer u) + widthOfPlayer/2
    aiLeft = playerWidth (universeRobot u) - widthOfPlayer/2
    aiRight = playerWidth (universeRobot u) + widthOfPlayer/2
    cannon = cannonWidth (universeCannon u)
    

-- |
cannonShot :: Float -> Universe -> Cannon
cannonShot dt u 
  | ((cannonRecharge cannon) >= timeOfRecharge) = Cannon
  { cannonWidth = (cannonWidth cannon)
  , cannonRecharge = 0
  , cannonBullets = updateBullets dt u (bullet : cannonBullets cannon)
}
                
  | otherwise = Cannon
  { cannonWidth = (cannonWidth cannon)
  , cannonRecharge = (cannonRecharge cannon) + dt
  , cannonBullets = updateBullets dt u (cannonBullets cannon)
}
  where 
    cannon = universeCannon u
    bullet = Bullet { bulletWidth = cannonWidth (universeCannon u)
  , bulletHeight = - 270}

-- | 
cannonShift :: Float -> Float -> Universe -> Cannon
cannonShift dt speed u = Cannon
  { cannonWidth = (cannonWidth cannon) + dt*speed
  , cannonRecharge = (cannonRecharge cannon) + dt
  , cannonBullets = updateBullets dt u (cannonBullets cannon)
}
  where
    cannon = universeCannon u


