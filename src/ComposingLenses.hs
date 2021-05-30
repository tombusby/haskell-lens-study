module ComposingLenses where

import Control.Lens

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
