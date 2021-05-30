module Main where

import Control.Lens

import qualified Lenses.ComposingLenses
import qualified Lenses.DataCorrection
import qualified Lenses.LensLaws
import qualified Lenses.Operators
import qualified Lenses.PolymorphicLenses
import qualified Lenses.VirtualFields

main :: IO ()
main = do
  Lenses.LensLaws.runTests
