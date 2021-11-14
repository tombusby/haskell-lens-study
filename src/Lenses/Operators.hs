module Lenses.Operators where

import Control.Lens
import Data.Char (toUpper)

data Payload = Payload
  { _weightKilos :: Int,
    _cargo :: String
  }
  deriving (Show)

data Ship = Ship
  { _payload :: Payload
  }
  deriving (Show)

makeLenses ''Payload
makeLenses ''Ship

serenity :: Ship
serenity = Ship (Payload 50000 "Livestock")

viewCargo :: String
viewCargo = view (payload . cargo) serenity

viewCargoInfix :: String
viewCargoInfix = serenity ^. payload . cargo

setCargo :: Ship
setCargo = set (payload . cargo) "Medicine" serenity

setCargoInfix :: Ship
setCargoInfix = serenity & payload . cargo .~ "Medicine"

setCargoGeneralised :: String -> Ship -> Ship
setCargoGeneralised = set $ payload . cargo

setMultipleInfix :: Ship
setMultipleInfix =
  serenity
    & payload . cargo .~ "Chocolate"
    & payload . weightKilos .~ 50

overInfixOperator :: Ship
overInfixOperator =
  serenity
    & payload . cargo .~ "Chocolate"
    & payload . weightKilos %~ subtract 1000

overInfixSubOperator :: Ship
overInfixSubOperator =
  serenity
    & payload . cargo .~ "Chocolate"
    & payload . weightKilos -~ 1000

data Thermometer = Thermometer
  { _temperature :: Int
  }
  deriving (Show, Eq)

makeLenses ''Thermometer

-- The < prefix makes it return a tuple with the NEW focus and NEW structure
getModifiedFocusAndStructure :: Bool
getModifiedFocusAndStructure =
  (Thermometer 20 & temperature <+~ 15) == (35, Thermometer 35)

-- The << prefix makes it return a tuple with the OLD focus and NEW structure
getOldFocusAndModifiedStructure :: Bool
getOldFocusAndModifiedStructure =
  (Thermometer 20 & temperature <<+~ 15) == (20, Thermometer 35)

-- Ex 1

data Gate = Gate
  { _open :: Bool,
    _oilTemp :: Float
  }
  deriving (Show)

data Army = Army
  { _archers :: Int,
    _knights :: Int
  }
  deriving (Show)

data Kingdom = Kingdom
  { _name :: String,
    _army :: Army,
    _gate :: Gate
  }
  deriving (Show)

makeLenses ''Gate
makeLenses ''Army
makeLenses ''Kingdom

duloc :: Kingdom
duloc =
  Kingdom
    { _name = "Duloc",
      _army =
        Army
          { _archers = 22,
            _knights = 14
          },
      _gate =
        Gate
          { _open = True,
            _oilTemp = 10.0
          }
    }

-- Using AND op to avoid %~ and .~ as per "hard mode"
goalA :: Kingdom
goalA =
  duloc
    & name <>~ ": a perfect place"
    & army . knights *~ 3
    & gate . open &&~ False

goalB :: Kingdom
goalB =
  duloc
    & name <>~ "instein"
    & army . archers -~ 5
    & army . knights +~ 12
    & gate . oilTemp ^~ 2

goalC :: (String, Kingdom)
goalC =
  duloc
    & name <>~ ": Home"
    & name <<<>~ " of the talking Donkeys"
    & _2 . gate . oilTemp -~ 5

goalC' :: (String, Kingdom)
goalC' =
  duloc
    & name <<>~ ": Home"
    & _2 . name <>~ " of the talking Donkeys"
    & _2 . gate . oilTemp //~ 2

-- Ex 2

flipTheBool :: Bool
flipTheBool = ((False, "opossums") & _1 ||~ True) == (True, "opossums")

multiplyNum :: Bool
multiplyNum = (2 & id *~ 3) == 6

dudleyIsTheWorst :: Bool
dudleyIsTheWorst = result == ((False, "DUDLEY - THE WORST"), 20.0)
  where
    result =
      ((True, "Dudley"), 55.0)
        & _1 . _2 <>~ " - the worst"
        & _2 -~ 15
        & _2 //~ 2
        & _1 . _2 %~ map toUpper
        & _1 . _1 &&~ False

-- Ex 3

-- view only takes two args

-- Ex 4

-- (%~) :: Lens s t a b -> (a -> b) -> s -> t
