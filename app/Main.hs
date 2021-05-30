module Main where

import Control.Lens

import qualified LensLaws
import qualified VirtualFields

main :: IO ()
main = do
  LensLaws.runTests
