module VirtualFields where

import Control.Lens

data Temperature = Temperature
  { _location :: String
  , _celsius :: Float
  }
  deriving (Show)

makeLenses ''Temperature

celsiusToFahrenheit :: Float -> Float
celsiusToFahrenheit c = (c * (9/5)) + 32

fahrenheitToCelsius :: Float -> Float
fahrenheitToCelsius f = (f - 32) * (5/9)

fahrenheit :: Lens' Temperature Float
fahrenheit = lens getter setter
  where
    getter = celsiusToFahrenheit . view celsius
    setter struct val = set celsius (fahrenheitToCelsius val) struct
