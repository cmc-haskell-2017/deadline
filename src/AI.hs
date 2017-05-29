module AI where

import Types
import Collides
import TypesAI
import Update
import Cannon
import Estimate
 
-- | Обновить состояние ИИ.
updateRobot :: Float -> Universe -> Player
updateRobot dt u = newAI dt u (definitionOfStrategy dt u)

-- | Определить нужную стратегию.
definitionOfStrategy :: Float -> Universe -> Strategy
definitionOfStrategy dt u 
  | estimateScreenUp dt u = newStrategy FromPlatform (0, 0, -1) False
  | estimateBullet dt u = newStrategy FromBullet (0, 0, -1) False
  | otherwise = newStrategy OnPlatform (0, 0, -1) False

-- | Выбрать соответствующую стратегию.
newAI :: Float -> Universe -> Strategy -> Player
newAI dt u strategy 
  | falseTransition (playerStrategy (universeRobot u)) = strategyOnPlatform dt u
  | (enumStrategy strategy == OnPlatform) && (enumStrategy (playerStrategy (universeRobot u)) == FromPlatform) = strategyfalseTransition dt u
  | enumStrategy strategy == OnPlatform = strategyOnPlatform dt u 
  | enumStrategy strategy == FromPlatform = strategyFromPlatform dt u
  | enumStrategy strategy == FromBullet = strategyFromBullet dt u

-- |Реализовать стратегию ложного перехода.
strategyfalseTransition :: Float -> Universe -> Player
strategyfalseTransition dt u
  | offset + 3 * platformHeight/4 < playerHeight (universeRobot u) - heightOfPlayer = falseTransitionWhile dt u
  | otherwise = falseTransitionEnd dt u
  where 
    (width, offset, life) = strategyPlatform (playerStrategy (universeRobot u))

-- | Реализовать стратегию ложного перехода, пока не исчезнет возможность взобраться обратно на платформу.
falseTransitionWhile :: Float -> Universe -> Player
falseTransitionWhile dt u = ai dt (helpAI u 0 (newStrategy FromPlatform (width, offset + dt * speed, life) False)) u
  where 
    (width, offset, life) = strategyPlatform (playerStrategy (universeRobot u))

-- | Реализовать стратегию ложного перехода. Завершение стратегии.
falseTransitionEnd :: Float -> Universe -> Player
falseTransitionEnd dt u = ai dt (helpAI u 0 (newStrategy FromPlatform (width, offset + dt * speed, life) True)) u
  where 
    (width, offset, life) = strategyPlatform (playerStrategy (universeRobot u))    

-- | Реализовать стратегию на платформу.
strategyOnPlatform :: Float -> Universe -> Player
strategyOnPlatform dt u 
  | fst (isWithPlatform dt (universeRobot u) u) = ai dt (helpAI u  0 (newStrategy OnPlatform platform False)) u
  | (enumStrategy (playerStrategy (universeRobot u)) == FromPlatform) = strategyHelp dt u
  | (width + platformWidth/2 < playerWidth (universeRobot u) - widthOfPlayer/2) = ai dt (helpAI u (-speed) (newStrategy OnPlatform (width, offset, life) False)) u
  | (width - platformWidth/2 > playerWidth (universeRobot u) + widthOfPlayer/2) = ai dt (helpAI u speed (newStrategy OnPlatform (width, offset, life) False)) u
  | otherwise = ai dt (helpAI u 0 (newStrategy OnPlatform (width, offset, life) False)) u
  where 
    platform = thisPlatform dt (universeRobot u) (universePlatforms u)
    (width, offset, life) = helpThisPlatform dt u
     

-- | Обновление предпочтительной платформы.
helpThisPlatform :: Float -> Universe -> Platform
helpThisPlatform dt u
  | (o + platformHeight/2 > (playerHeight (universeRobot u)) - heightOfPlayer/2) || (l < 0) = (w1, o1 + dt * speed, l1)
  | otherwise = (w, o + dt * speed, l)
  where
    (w, o, l) = strategyPlatform (playerStrategy (universeRobot u))
    (w1, o1, l1) = bestPlatformWithout dt (universeRobot u) (strategyPlatform (playerStrategy (universeRobot u))) (mapPlatforms dt (universeRobot u) (universePlatforms u)) 

-- | Реализовать стратегию на платформу, если до этого ИИ имел стратегию уйти с платформы.
strategyHelp :: Float -> Universe -> Player
strategyHelp dt u
  | (width + platformWidth/2 < playerWidth player - widthOfPlayer/2) = ai dt (helpAI u (- speed) (newStrategy OnPlatform (width, offset + dt * speed, life) False)) u
  | (width - platformWidth/2 > playerWidth player + widthOfPlayer/2) = ai dt (helpAI u speed (newStrategy OnPlatform (width, offset + dt * speed, life) False)) u
  | otherwise = ai dt (player {playerSpeed = 0, playerStrategy = newStrategy OnPlatform (width, offset + dt * speed, life) False}) u
  where
    player = universeRobot u
    (width, offset, life) = bestPlatformWithout dt (universeRobot u) (strategyPlatform (playerStrategy (universeRobot u))) (mapPlatforms dt (universeRobot u) (universePlatforms u)) 

-- | Реализовать стратегию уйти с платформы.
strategyFromPlatform :: Float -> Universe -> Player
strategyFromPlatform dt u  
  | life1 < 0 = ai dt (helpAI u 0 (newStrategy FromPlatform (width, offset + dt * speed, life) False)) u
  | width < (playerWidth (universeRobot u)) = ai dt (helpAI u speed (newStrategy FromPlatform (width, offset + dt * speed, life) False)) u
  | otherwise = ai dt (helpAI u (- speed) (newStrategy FromPlatform (width, offset + dt * speed, life) False)) u
  where 
    (width, offset, life) = thisPlatform dt (universeRobot u) (universePlatforms u)
    (width1, offset1, life1) = bestPlatformWithout dt (universeRobot u) (thisPlatform dt (universeRobot u) (universePlatforms u)) (mapPlatforms dt (universeRobot u) (universePlatforms u)) 

-- | Реализовать стратегию уйти от пули.
strategyFromBullet :: Float -> Universe -> Player
strategyFromBullet dt u 
  | width < (playerWidth (universeRobot u)) = ai dt (helpAI u (- speed) (newStrategy FromBullet (width, offset + dt * speed, life) False)) u
  | otherwise = ai dt (helpAI u speed (newStrategy FromBullet (width, offset + dt * speed, life) False)) u
  where 
    (width, offset, life) = strategyPlatform (playerStrategy (universeRobot u))

-- | Обновить скорость и стратегию ИИ.
helpAI :: Universe -> Float -> Strategy -> Player
helpAI u sp strategy = (universeRobot u){
  playerSpeed = sp, 
  playerStrategy = strategy
}

-- | Обновить стратегию ИИ.
newStrategy :: EnumStrategy -> Platform -> Bool -> Strategy
newStrategy enum platform fT = Strategy {
  enumStrategy = enum, 
  strategyPlatform = platform, 
  falseTransition = fT
}

-- | Предполагаемое обновление ИИ.
ai :: Float -> Player -> Universe -> Player
ai dt aiplayer u
  | fst (isWithPlatform dt (universeRobot u) u) =  keepPlayer dt aiplayer
  | snd (isWithPlatform dt (universeRobot u) u) =  holdPlayer dt aiplayer
  | otherwise = updatePlayer dt aiplayer
