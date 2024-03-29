{-# LANGUAGE NamedFieldPuns #-}

module Lenses.DataCorrection where

import Control.Lens
import Prelude hiding (round)

data Time = Time
  { _hours :: Int
  , _mins :: Int
  }
  deriving (Show)

-- using this is useful in that we can prevent invalid states but it does
-- produce unlawful lenses. This is probably ok but worth thinking about.
clamp :: Int -> Int -> Int -> Int
clamp minVal maxVal = min maxVal . max minVal

hours :: Lens' Time Int
hours = lens getter setter
 where
  getter (Time h _) = h
  -- Take the hours 'mod' 24 so we always end up in the right range
  -- We can't use getters and setters because it would involve infinitely
  -- mutually recursive types to resolve it.
  setter (Time _ m) newHours = Time (newHours `mod` 24) m

mins :: Lens' Time Int
mins = lens getter setter
 where
  getter (Time _ m) = m
  -- Minutes overflow into hours
  setter (Time h _) newMinutes =
    -- We can't use getters and setters because it would involve infinitely
    -- mutually recursive types to resolve it.
    Time ((h + (newMinutes `div` 60)) `mod` 24) (newMinutes `mod` 60)

data ProducePrices = ProducePrices
  { _limePrice :: Float
  , _lemonPrice :: Float
  }
  deriving (Show)

round :: Float -> Float
round = max 0.0

limePrice :: Lens' ProducePrices Float
limePrice = lens getter setter
 where
  getter = _limePrice
  setter pp@ProducePrices{_lemonPrice} price =
    let correctedPrice = round price
        correctedLemonPrice =
          if correctedPrice > _lemonPrice
            then max _lemonPrice $ correctedPrice - 0.50
            else min _lemonPrice $ correctedPrice + 0.50
     in pp
          { _limePrice = round correctedPrice
          , _lemonPrice = correctedLemonPrice
          }

lemonPrice :: Lens' ProducePrices Float
lemonPrice = lens getter setter
 where
  getter = _lemonPrice
  setter pp@ProducePrices{_limePrice} price =
    let correctedPrice = round price
        correctedLimePrice =
          if correctedPrice > _limePrice
            then max _limePrice $ correctedPrice - 0.50
            else min _limePrice $ correctedPrice + 0.50
     in pp
          { _lemonPrice = round correctedPrice
          , _limePrice = correctedLimePrice
          }
