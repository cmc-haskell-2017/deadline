module TypesAI where

import Types


-- | Дерево игры.
data GameTree a = Node [(Move, GameTree a)] | Leaf a

-- | Движение.
data Move = Move
  { moveWidth :: Width
  , moveJump :: Bool
  , moveUniverse :: Universe
  } 

-- | Глубина дерева. Можно связать со сложностью ИИ!
treeDepth :: Int
treeDepth = 6

instance Functor GameTree where
gtmap :: (a -> b) -> GameTree a -> GameTree b
gtmap f (Node trees) = Node (map g trees)
  where g (move, tree) = (move, gtmap f tree)
gtmap f (Leaf u) = Leaf (f u)

-- | Оценка.
type Estimate = Float

-- | 
data BestMove = BestMove Move Estimate | NoMove 

-- |
maxS :: Float
maxS = sqrt (fromInteger (toInteger (screenHeight * screenHeight + screenWidth * screenWidth)))

