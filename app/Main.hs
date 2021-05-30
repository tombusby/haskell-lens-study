module Main where

import Control.Lens

import qualified LensLaws

main :: IO ()
main = do
  LensLaws.runTests
