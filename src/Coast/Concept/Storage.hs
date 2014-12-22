{-# LANGUAGE ConstraintKinds #-}
module Coast.Concept.Storage 
( Goods,
  Quantity,
  LogisticsResult(..),
  Storage(..) )
where

import Control.Monad.State

type Goods = String
type Quantity = Integer

data LogisticsResult = Fully | Partially | None
    deriving (Eq, Show)

class Storage a where
    amountOf :: Goods -> State a Quantity
    give :: Goods -> Quantity -> State a LogisticsResult
    take :: Goods -> Quantity -> State a LogisticsResult
    canTake :: Goods -> Quantity -> State a Bool
    canTake g q = liftM (>= q) $ amountOf g
