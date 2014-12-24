module Coast.Town.PoliticalEntities where

import Coast.Town.Warehouse
import Coast.Utils.Percented
import Coast.Wealth.Economy

data PopulationLevel = Village | Outpost | Burg | Town | City | Metropolis
                       deriving (Eq, Show, Bounded, Enum)

data Kingdom = Kingdom { _kname :: String,
                         _cities :: [Settlement] }

data PoliticalRegime = 
    Subordinated 
  | Aristocracy 
  | Autocracy 
  | Patriarcy 
  | Democracy
   deriving (Eq, Show, Enum, Bounded)

type Classes = [Quantifier SocialClass]
data SocialClass
    = Citizens -- Simple people, workers. Change cost of labour.
    | Merchants -- Trader, invester, industry captain. Change industry & trade.
    | Administration -- Lawyers, regulators. Change laws, taxes, loopholes...
    | Supernatural -- Mages, demonic invocators... Provoke huge spikes.
    | Aristocrats -- Rulers. Increase wars and demands for luxury.
    | Thieves -- Mischievous felons. Increase smuggling, underground guilds...

data Allegiance = Indendant | Inside Kingdom

type Building = [Product]

data Settlement= Settlement { _tname :: String,
                              _population :: PopulationLevel,
                              _storage :: Warehouse,
                              _buildings :: [Building],
                              _sociology :: Classes,
                              _allegiance :: Allegiance,
                              _politics :: PoliticalRegime }
