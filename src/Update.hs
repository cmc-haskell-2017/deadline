module Update where

import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Line
import Graphics.Gloss.Interface.Pure.Game
import Types
import Draw
import Init

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
    
-- | Обновить состояние игровой вселенной.
updateUniverse :: Float -> Universe -> Universe
updateUniverse dt u
  | isGameOver u = u { universeGameOver = Just initGameOver } -- resetUniverse g u
  | otherwise = (upUniverse dt u) {universePlayer = updatePlayer dt (universePlayer u) u}
 
upUniverse:: Float -> Universe -> Universe 
upUniverse dt u = u { universePlatforms  = updatePlatforms  dt (universePlatforms  u)
      , universeScore  = (universeScore u) + dt
      }

-- | Конец игры?
isGameOver :: Universe -> Bool
isGameOver u = playerBelowFloor || playerBelowRoof
  where
    playerBelowRoof = playerHeight (universePlayer u) >  screenUp - 30
    playerBelowFloor = playerHeight (universePlayer u) < screenDown + 30

-- | Сталкивается ли игрок с любыми из
-- бесконечного списка ворот?
collision :: Float -> Player -> [Platform] -> (Height, Float)
collision dt player [] = (playerHeight player + dt *( playerFallingSpeed player + gravity/2 * dt) , playerFallingSpeed player + gravity/2 *dt)
collision dt player platforms = newHeightPlayer (map (collides dt player) (takeWhile onScreen (absolutePlatforms platforms))) noPlatform
  where
    onScreen (_, offset) = offset - platformHeight > screenDown
    noPlatform = (playerHeight player + dt *(playerFallingSpeed player + gravity/2 * dt) , playerFallingSpeed player + gravity/2 *dt)

newHeightPlayer :: [Maybe a] -> a -> a
newHeightPlayer [] platform = platform
newHeightPlayer (Nothing : xs) p = newHeightPlayer xs p
newHeightPlayer ((Just new) : xs) _ = new

collide :: Float -> Player -> Platform -> Bool
collide dt player platform = and [
  ((playerHeight player - heigthOfPlayer - dt * (playerFallingSpeed player)) < (snd platform + platformHeight + speed * dt)), 
  ((playerHeight player - heigthOfPlayer) > (snd platform - platformHeight)), 
  ((playerWidth player - widthOfPlayer) < (fst platform + platformWidth /2)), 
  ((playerWidth player + widthOfPlayer) > (fst platform - platformWidth /2))
  ]
  where heigthOfPlayer = 1200*0.03
        widthOfPlayer = 800*0.03

collidesNear :: Player -> Platform -> Bool
collidesNear player platform = or[ 
  polygonBoxCollides polygon box
  | polygon <- playerPolygons player
  , box <- platformBoxes platform ]

polygonBoxCollides :: Path -> (Point, Point) -> Bool
polygonBoxCollides xs (lb, rt) = or
  [ not (segClearsBox p1 p2 lb rt)
  | (p1, p2) <- zip xs (tail (cycle xs)) ]



-- |  Становится ли игрок на платформу?
collides :: Float -> Player -> Platform -> Maybe (Height, Float)
collides dt player platform |collide dt player platform = (Just ( snd platform + platformHeight + speed * dt + heigthOfPlayer, speed *dt))
                            |otherwise = Nothing
  where heigthOfPlayer = 1200*0.03
   

inPlatform :: Float -> Player -> Float -> [Platform] -> Width
inPlatform dt player wid [] = wid
inPlatform dt player wid platforms = newHeightPlayer (map (inplat dt player wid) (takeWhile onScreen (absolutePlatforms platforms))) wid
  where
    onScreen (_, offset) = offset - platformHeight > screenDown

inplat :: Float -> Player -> Float -> Platform -> Maybe Width
inplat dt player wid platform |and [  
  not (collide dt player platform),
  collidesNear player platform
  ] = (Just (playerWidth player))
                         |otherwise = Nothing
   where widthOfPlayer = 800*0.03

-- | Обновить состояние игрока.
-- Игрок не может прыгнуть выше потолка.
 -- | Обновить состояние игрока.

keepPlayerOnScreen :: Float -> Player -> [Platform] -> Player 
keepPlayerOnScreen dt player platform = player {
  playerWidth = inPlatform dt player width platform
} 
  where
    width = (max (min w (playerWidth player) + dt * (playerSpeed player)) wm)
    w = 200
    wm = -200 

updatePlayer :: Float -> Player -> Universe -> Player
updatePlayer dt player u = (keepPlayerOnScreen dt player (universePlatforms u)) {
  playerHeight = fst height,
  playerFallingSpeed = snd height
}
  where height = collision dt player (universePlatforms u)
