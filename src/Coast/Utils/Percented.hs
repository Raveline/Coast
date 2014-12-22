module Coast.Utils.Percented where

import qualified Data.Map as M

type Percentage = Float
type Quantifier a = M.Map a Int
type Percented a = M.Map a Percentage

quantifiedToPercented :: Quantifier a -> Percented a
quantifiedToPercented m = M.map asPercent m
    where total = M.foldl' (+) 0 m
          asPercent :: (Integral a) => a -> Float
          asPercent n = (fromIntegral n * 100) / fromIntegral total
