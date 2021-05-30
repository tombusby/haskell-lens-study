module ComposingLenses where

import Control.Lens

-- Example 1

data Person = Person
  { _name    :: String
  , _address :: Address
} deriving (Show)

data Address = Address
  { _streetAddress :: StreetAddress
  , _city          :: String
  , _country       :: String
  }
  deriving (Show)

data StreetAddress = StreetAddress
  { _streetNumber :: String
  , _streetName   :: String
  } deriving (Show)

makeLenses ''Person
makeLenses ''Address
makeLenses ''StreetAddress

sherlock :: Person
sherlock = Person
  { _name = "S. Holmes" , _address = Address
    { _streetAddress = StreetAddress {
        _streetNumber = "221A"
      , _streetName = "Baker Street"
      }
    , _city = "London"
    , _country = "England"
    }
  }

-- Update a Person's Address
updateAddress :: (Address -> Address) -> (Person -> Person)
updateAddress modify existingPerson =
  existingPerson
    { _address = modify . _address $ existingPerson
    }

-- Update a Street Address within an Address
updateStreetAddress :: (StreetAddress -> StreetAddress) -> (Address -> Address)
updateStreetAddress modify existingAddress =
  existingAddress
    { _streetAddress = modify . _streetAddress $ existingAddress
    }

-- Update a Street Number within a Streed Address
updateStreetNumber :: (String -> String) -> (StreetAddress -> StreetAddress)
updateStreetNumber modify existingStreetAddress =
  existingStreetAddress
    { _streetNumber = modify . _streetNumber $ existingStreetAddress
    }

setPersonStreetNumber :: String -> Person -> Person
setPersonStreetNumber =
  updateAddress . updateStreetAddress . updateStreetNumber . const


-- Example 2

-- Some dead-simple types which represent our game
data Player  = Player  deriving Show
data Wool    = Wool    deriving Show
data Sweater = Sweater deriving Show

data Item a = Item
  { _material :: a
  , _amount :: Int
  }
  deriving Show

makeLenses ''Player
makeLenses ''Wool
makeLenses ''Sweater
makeLenses ''Item

weave :: Wool -> Sweater
weave Wool = Sweater

gameState :: (Player, Item Wool)
gameState = (Player, Item Wool 5)

updatedGameState :: (Player, Item Sweater)
updatedGameState = over (_2 . material) weave gameState


-- Ex 1

waldo :: Bool
waldo =
  view (_2 . _1 . _2) ("Ginerva", (("Galileo", "Waldo"), "Malfoy")) == "Waldo"

-- Ex 2

data Five
data Eight
data Two
data Three

fiveEightDomino :: Lens' Five Eight
fiveEightDomino = undefined

mysteryDomino :: Lens' Eight Two
mysteryDomino = undefined

twoThreeDomino :: Lens' Two Three
twoThreeDomino = undefined

dominoTrain :: Lens' Five Three
dominoTrain = fiveEightDomino . mysteryDomino . twoThreeDomino

-- Ex 3

-- l :: Functor f => (Armadillo -> f Hedgehog) -> (Platypus -> f BabySloth)
-- is equivalent to a lens of:
-- l :: Lens Platypus BabySloth Armadillo Hedgehog

-- Ex 4

data Chumble
data Spuzz
data Gazork
data Trowlg
data Bandersnatch
data Yakka
data Zink
data Wattoom
data Grug
data Pubbawup
data Foob
data Mog
data Boojum
data Jabberwock
data Snark
data JubJub

spuzorktrowmble   :: Lens Chumble      Spuzz      Gazork       Trowlg
spuzorktrowmble   =  undefined

gazorlglesnatchka :: Lens Gazork       Trowlg     Bandersnatch Yakka
gazorlglesnatchka =  undefined

zinkattumblezz    :: Lens Zink         Wattoom    Chumble      Spuzz
zinkattumblezz    =  undefined

gruggazinkoom     :: Lens Grug         Pubbawup   Zink         Wattoom
gruggazinkoom     =  undefined

banderyakoobog    :: Lens Bandersnatch Yakka      Foob         Mog
banderyakoobog    =  undefined

boowockugwup      :: Lens Boojum       Jabberwock Grug         Pubbawup
boowockugwup      =  undefined

snajubjumwock     :: Lens Snark        JubJub     Boojum       Jabberwock
snajubjumwock     =  undefined

allLenses :: Lens Snark JubJub Foob Mog
allLenses
  = snajubjumwock
  . boowockugwup
  . gruggazinkoom
  . zinkattumblezz
  . spuzorktrowmble
  . gazorlglesnatchka
  . banderyakoobog
