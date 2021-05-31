module Main where

import Control.Lens

import qualified Lenses.ComposingLenses
import qualified Lenses.DataCorrection
import qualified Lenses.LensLaws
import qualified Lenses.Operators
import qualified Lenses.PolymorphicLenses
import qualified Lenses.VirtualFields

import qualified Folds.Folds
import qualified Folds.CustomFolds

main :: IO ()
main = do
  Lenses.LensLaws.runTests
