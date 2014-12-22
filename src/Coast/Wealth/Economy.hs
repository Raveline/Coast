module Coast.Wealth.Economy
where

import Coast.Concept.Production
import Coast.Concept.Storage

type Cost = Float

data PrimaryProductCondition 
    = Biome
    | Altitude
    deriving(Eq)

data Product = 
    PrimaryProduct { _pname :: String 
                    ,_extractionSpeed :: UnitPerTick
                    ,_conditions :: [PrimaryProductCondition]
                    ,_baseCost :: Float }
    | IndustryProduct { _pname :: String
                       ,_productionSpeed :: UnitPerTick
                       ,_depends :: [(Product, Quantity)]
                       ,_baseCost :: Float }

-- data Service = 
--    Service { _sname :: String
--            , _consumes :: [Product]
--            , _sbaseCost :: Float }

instance Show Product where
    show = _pname

-- | Product is our main way to "build" things in a town.
-- >>> let p = PrimaryProduct "Wood" 1 [] 0
-- >>> need p
-- []
-- >>> let p' = IndustryProduct "Furniture" 1 [(p, 1)] 10
-- >>> need p'
-- [(Wood,1)]
-- >>> import qualified Data.Map as M
-- >>> import Coast.Town.Warehouse
-- >>> import Coast.Concept.Storage
-- >>> import Control.Monad.State
-- >>> let w = Warehouse 200 $ M.fromList [("Wood", 1)]
-- >>> let r = execState (produce p') w
-- >>> evalState (amountOf "Furniture") r
-- 1
instance Productable Product where
    need ip@(IndustryProduct{}) = _depends ip
    need _ = []
