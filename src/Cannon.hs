module Cannon where

import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Interface.Pure.Game
import System.Random
import Graphics.Gloss.Juicy
import Types

-- | 
updateCannon :: Float -> Universe -> Cannon
updateCannon dt u | under = cannonShot dt u
                  | left = cannonShift dt cannonNormSpeed u
                  | right = cannonShift dt (-cannonNormSpeed) u
  where 
    (under, left, right) = whereCannon u

-- |
whereCannon :: Universe -> (Bool, Bool, Bool)
whereCannon u = (
  and 
    [ playerLeft < cannon
    , playerRight > cannon],
  playerLeft > cannon,
  playerRight < cannon)
  where
    playerLeft = playerWidth (universePlayer u) - widthOfPlayer/2
    playerRight = playerWidth (universePlayer u) + widthOfPlayer/2
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

-- | 
updateBullets :: Float -> Universe -> [Bullet] -> [Bullet]
updateBullets _ _ [] = []
updateBullets dt u (bullet : bullets) 
  | screenUp < (bulletHeight bullet) = updateBullets dt u bullets
  | otherwise = (Bullet { bulletWidth = (bulletWidth bullet) , bulletHeight = (bulletHeight bullet) + dt*bulletSpeed}) : (updateBullets dt u bullets)



