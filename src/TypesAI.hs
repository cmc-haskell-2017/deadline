module TypesAI where

import Types 

-- | Диагональ экрана.
maxS :: Float
maxS = sqrt (fromInteger (toInteger (screenHeight * screenHeight + screenWidth * screenWidth)))
