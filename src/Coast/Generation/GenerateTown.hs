{-# LANGUAGE GeneralizedNewtypeDeriving #-}  
module Coast.Generation.Town where

import Control.Monad.Random
import Coast.Town.Warehouse
import Coast.Town.PoliticalEntities
import qualified Data.Map as M



newtype Enum' a = Enum' a deriving (Bounded, Enum, Show)

instance (Enum a, Bounded a) => Random (Enum' a) where
  random = enumRandom
  randomR = enumRandomR

instance (Enum a, Bounded a) => Arbitrary (Enum' a) where
  arbitrary = choose (minBound, maxBound)



-- This will create a random city
generateTown :: (MonadRandom r) => r Settlement
generateTown = do name <- generateName
                  size <- generateSize
                  storage <- return $ generateStorage size
                  classes <- generateClasses
                  allegiance <- return Indendant
                  politics <- generatePolitics
                  return $ Settlement name size storage [] classes allegiance politics
        
        
generateClasses :: (MonadRandom r) => r Classes
generateClasses = undefined

generatePolitics :: (MonadRandom r) => r PoliticalRegime
generatePolitics = getRandomR (Aristocracy, Democracy)

generateSize :: (MonadRandom r) => r PopulationLevel
generateSize = getRandomR (minBound :: PopulationLevel, maxBound :: PopulationLevel)

generateStorage :: PopulationLevel -> Warehouse
generateStorage Village = Warehouse 50 M.empty
generateStorage Outpost = Warehouse 200 M.empty
generateStorage Burg = Warehouse 500 M.empty
generateStorage Town = Warehouse 1500 M.empty
generateStorage City = Warehouse 3000 M.empty
generateStorage Metropolis = Warehouse 10000 M.empty

generateName :: (MonadRandom r) => r String
generateName = return "Exemple"
