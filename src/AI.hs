module AI where

import Types
import Collides
import TypesAI
import Update
import Cannon
import Estimate
 
-- | Обновить состояние ИИ.
updateRobot :: Float -> Universe -> Player
updateRobot dt u = (newAI (universeRobot u) dt u (betterMove (bestMoves (getEstimate dt (gameTree treeDepth u dt)))))

-- | Создать дерево игры глубины n.
gameTree :: Int -> Universe -> Float -> GameTree Universe
gameTree 1 u _ = Leaf u
gameTree n u dt 
  | not (universePlay u) = Leaf u
  | otherwise = Node ([ (Move { moveWidth = 0, moveJump = False, moveUniverse = universeNo}, gameTree (n - 1) universeNo dt)]
                     ++ (maybeRight n dt u)
                     ++ (maybeLeft n dt u) 
                     ++ (maybeJump n dt u))
  where 
    universeNo = (maybeUniverse u dt 0)
    universeRight = (maybeUniverse u dt bumpSpeed)
    universeLeft = (maybeUniverse u dt (-bumpSpeed))

-- |
maybeJump :: Int -> Float -> Universe -> [(Move, GameTree Universe)]
maybeJump n dt u 
  | (fst (isWithPlatform dt (universeRobot u) u)) && (not (collidesHelper (playerSquare (universePlayer u) dt) (playerSquare (universeRobot u) dt))) = (Move { moveWidth = 0, moveJump = True, moveUniverse = universeJump}, gameTree (n - 1) universeJump dt) : []
  | otherwise = []
  where universeJump = maybeUniverse (bumpUp u) dt 0

-- |
maybeLeft :: Int -> Float -> Universe -> [(Move, GameTree Universe)]
maybeLeft n dt u 
  | collidesHelper (rotateLeft (playerSquare (universeRobot u) dt)) (rotateLeft (playerSquare (universePlayer u) dt)) = []
  | otherwise = [(Move { moveWidth = -bumpSpeed, moveJump = False, moveUniverse = universeLeft}, gameTree (n - 1) universeLeft dt)]
  where universeLeft = (maybeUniverse u dt (-bumpSpeed))

-- |
maybeRight :: Int -> Float -> Universe -> [(Move, GameTree Universe)]
maybeRight n dt u 
  | collidesHelper (rotateRight (playerSquare (universeRobot u) dt)) (rotateRight (playerSquare (universePlayer u) dt)) = []
  | otherwise = [(Move { moveWidth = bumpSpeed, moveJump = False, moveUniverse = universeRight}, gameTree (n - 1) universeRight dt)]
  where universeRight = (maybeUniverse u dt bumpSpeed)


-- | Сдвинуть игрока вверх.
bumpUp :: Universe -> Universe
bumpUp u = u
  { universeRobot = (universeRobot u) { playerFallingSpeed = jumpSpeed }
  }

-- | Предполагаемое обновление вселенной.
maybeUniverse :: Universe -> Float -> Float -> Universe
maybeUniverse u dt speed = newUniverse { universePlay = not (isGameOver dt (universeRobot newUniverse) newUniverse)}
  where newUniverse = (maybeUniverseHelp u dt speed)

-- |
maybeUniverseHelp :: Universe -> Float -> Float -> Universe
maybeUniverseHelp u dt speed = Universe{
  universePlayer = universePlayer u,
  universePlatforms = updatePlatforms dt (universePlatforms u) u,                 
  universeCannon = (universeCannon u){ cannonBullets = updateBullets dt u (cannonBullets (universeCannon u))},
  universeRobot = ai dt ((universeRobot u){playerSpeed = speed}) u
}

-- | Предполагаемое обновление ИИ.
ai :: Float -> Player -> Universe -> Player
ai dt aiplayer u
  | fst (isWithPlatform dt (universeRobot u) u) =  keepPlayer dt aiplayer
  | snd (isWithPlatform dt (universeRobot u) u) =  holdPlayer dt aiplayer
  | otherwise = updatePlayer dt aiplayer

-- |
bestMoves :: GameTree Estimate -> GameTree BestMove
bestMoves (Leaf _) = Leaf NoMove
bestMoves (Node trees) = Node (map g trees)
  where g (move, tree) = (move, gtmap (\a -> (BestMove move a)) tree)

-- |
betterMove :: GameTree BestMove -> BestMove
betterMove (Leaf a) = a
betterMove (Node tree) = maxEstimate (map g tree)
  where g (_, btree) = betterMove btree

-- |
maxEstimate :: [BestMove] -> BestMove
maxEstimate [] = NoMove
maxEstimate (x: []) = x
maxEstimate (NoMove : xs) = maxEstimate xs
maxEstimate (x : NoMove : xs) = maxEstimate (x : xs)
maxEstimate ((BestMove m1 e1) : (BestMove m2 e2): ss)
  | e1 >= e2 = maxEstimate ((BestMove m1 e1) : ss)
  | otherwise = maxEstimate ((BestMove m2 e2) : ss)

-- |
newAI :: Player -> Float -> Universe -> BestMove -> Player 
newAI aiplayer dt u (NoMove) = ai dt (aiplayer{playerSpeed = 0}) u
newAI aiplayer dt u (BestMove move _) 
  | moveJump move = ai dt (aiplayer{playerSpeed = moveWidth move, playerFallingSpeed = jumpSpeed}) u
  | otherwise = ai dt (aiplayer{playerSpeed = moveWidth move}) u
  

