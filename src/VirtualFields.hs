module VirtualFields where

import Control.Lens

data Temperature = Temperature
  { _location :: String
  , _kelvin :: Float
  }
  deriving (Show)

makeLenses ''Temperature

celsiusToFahrenheit :: Float -> Float
celsiusToFahrenheit c = (c * (9/5)) + 32

fahrenheitToCelsius :: Float -> Float
fahrenheitToCelsius f = (f - 32) * (5/9)

-- We originally built the Temperature type with a _celcius field and then
-- provided this fahrenheit field
fahrenheit :: Lens' Temperature Float
fahrenheit = lens getter setter
  where
    getter = celsiusToFahrenheit . view celsius
    setter struct val = set celsius (fahrenheitToCelsius val) struct

-- After switching to the new Kelvin format for the base record type, we simply
-- need to provide another virtual field for celsius to keep everything working
celsius :: Lens' Temperature Float
celsius = lens getter setter
  where
    getter = subtract 273.15 . view kelvin
    setter temp c = set kelvin (c + 273.15) temp
