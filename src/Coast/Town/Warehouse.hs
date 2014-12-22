{-# LANGUAGE TemplateHaskell #-}
module Coast.Town.Warehouse where

import qualified Data.Map as Map
import Coast.Concept.Storage
import Data.Maybe
import Control.Lens
import Control.Monad.State

data Warehouse = Warehouse { _size :: Quantity
                           , _content :: Map.Map Goods Quantity }
    deriving (Eq, Show)

$(makeLenses ''Warehouse)

-- | Compute the used space in this warehouse
-- >>> let g = Map.fromList[("productA", 10), ("productB", 10)]
-- >>> let w = Warehouse 30 g
-- >>> evalState usedSpace w
-- 20
usedSpace :: State Warehouse Quantity
usedSpace = liftM (sumOf folded) $ use content 

-- | Check the available room for storage in a warehouse
-- >>> let g = Map.fromList[("productA", 10), ("productB", 10)]
-- >>> let w = Warehouse 30 g
-- >>> evalState availableSpace w
-- 10
availableSpace :: State Warehouse Quantity
availableSpace = do available <- use size
                    used <- usedSpace
                    return $ available - used

-- | Read the number of given Goods inside the warehouse
-- >>> let g = Map.fromList[("productA", 10), ("productB", 5)]
-- >>> let w = Warehouse 30 g
-- >>> evalState (amountOf "productA") w
-- 10
-- >>> evalState (amountOf "productB") w
-- 5
-- >>> evalState (amountOf "productC") w
-- 0


-- | Add some product inside the warehouse. If there is not
-- enough room for every product, will try to fill as much as
-- possible. If there is no room available, will have no effect.
-- >>> let g = Map.fromList[("productA", 10), ("productB", 10)]
-- >>> let w = Warehouse 30 g
-- >>> evalState (give "productA" 5 >> amountOf "productA") w
-- 15
-- >>> evalState (give "productA" 5) w == Fully
-- True
-- >>> evalState (give "productB" 10 >> amountOf "productB") w
-- 20
-- >>> evalState (give "productC" 5 >> amountOf "productC") w
-- 5
-- >>> evalState (give "productA" 200 >> amountOf "productA") w
-- 20
-- >>> evalState (give "productA" 200) w == Partially
-- True
-- >>> evalState (give "productC" 200 >> amountOf "productC") w
-- 10
-- >>> let w' = Warehouse 20 g
-- >>> evalState (give "productA" 1 >> amountOf "productA") w'
-- 10
-- >>> evalState (give "productC" 1 >> amountOf "productC") w'
-- 0
-- >>> evalState (give "productC" 1) w' == None
-- True
instance Storage Warehouse where
    amountOf g = do c <- use content
                    return $ fromMaybe 0 (c^.at g)

    give goods quant = do available <- availableSpace
                          case available of
                            0 -> return None
                            x -> if quant > x
                                    then give' goods x >> return Partially
                                    else give' goods quant >> return Fully
         where give' :: Goods -> Quantity -> State Warehouse ()
               give' g q =  do amount <- amountOf g
                               let newValue = amount + q
                               changeContent newValue g

    take goods quant = do available <- amountOf goods
                          case available of
                            0 -> return None
                            x -> if quant > x
                                    then take' goods x >> return Partially
                                    else take' goods quant >> return Fully
        where take' :: Goods -> Quantity -> State Warehouse ()
              take' g q = do amount <- amountOf g 
                             let newValue = amount - q
                             changeContent newValue g

changeContent :: Quantity -> Goods -> State Warehouse ()
changeContent newValue g = do c <- use content
                              let newContent = at g .~ Just newValue $ c
                              content .= newContent
                              return ()
