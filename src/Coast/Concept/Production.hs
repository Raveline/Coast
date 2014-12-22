module Coast.Concept.Production
(UnitPerTick,
Productable (..))
where

import Coast.Concept.Storage 
import Control.Monad.State
import Control.Arrow
import Prelude hiding (take)

type UnitPerTick = Float

class (Show p) => Productable p where
    need :: p -> [(p,Quantity)]
    need2 :: p -> [(String, Quantity)]
    need2 = map (first show) . need

    produce :: (Storage s) => p -> State s ()
    produce p = liftM and (checkResourcesAvailableFor p)
                >>= flip when (productionProcess p)

checkResourcesAvailableFor :: (Storage s, Productable p) => p -> State s [Bool]
checkResourcesAvailableFor p = mapM (uncurry canTake) (need2 p)

productionProcess :: (Storage s, Productable p) => p -> State s ()
productionProcess item = mapM_ (uncurry take) (need2 item) 
                         >> give (show item) 1
                         >> return ()
