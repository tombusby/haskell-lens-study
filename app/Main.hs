module Main where

import Control.Lens

import qualified DataCorrection
import qualified LensLaws
import qualified PolymorphicLenses
import qualified VirtualFields

main :: IO ()
main = do
  LensLaws.runTests
