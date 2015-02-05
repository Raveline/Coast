module Coast.Generation.Town where

import Control.Monad
import Control.Monad.Random
import Coast.Town.Warehouse
import Coast.Town.PoliticalEntities
import qualified Data.Map as M

boundedEnumRandomR :: (Bounded a, Enum a, RandomGen g) =>
                      (a, a) -> g -> (a, g)
boundedEnumRandomR (x, y) g = case randomR (fromEnum x, fromEnum y) g of
                                (r, g') -> (toEnum r, g')


getRandomR' :: (MonadRandom m, Enum a, Bounded a) => (a,a) -> m a
getRandomR' (a, b) = uniform [a..b]

-- This will create a random city
generateTown :: (MonadRandom r) => r Settlement
generateTown = do name <- generateName
                  _size <- generateSize
                  let storage = generateStorage _size
                  let allegiance = Independant
                  classes <- generateClasses
                  politics <- generatePolitics
                  return $ Settlement name _size storage [] classes allegiance politics
        
generateClasses :: (MonadRandom r) => r Classes
generateClasses = do let allClasses = [minBound :: SocialClass ..]
                     pops <- replicateM (length allClasses) getInt
                     let zipped = zip allClasses pops
                     return $ M.fromList zipped

getInt :: (MonadRandom r) => r Int
getInt = getRandomR (0,100)
                     
generatePolitics :: (MonadRandom r) => r PoliticalRegime
generatePolitics = getRandomR' (Aristocracy, Democracy)

generateSize :: (MonadRandom r) => r PopulationLevel
generateSize = getRandomR' (minBound :: PopulationLevel,
                            maxBound :: PopulationLevel)

generateStorage :: PopulationLevel -> Warehouse
generateStorage Village = Warehouse 50 M.empty
generateStorage Outpost = Warehouse 200 M.empty
generateStorage Burg = Warehouse 500 M.empty
generateStorage Town = Warehouse 1500 M.empty
generateStorage City = Warehouse 3000 M.empty
generateStorage Metropolis = Warehouse 10000 M.empty

generateName :: (MonadRandom r) => r String
generateName = return "Exemple"
