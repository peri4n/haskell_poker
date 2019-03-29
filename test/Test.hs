module Main where

import           CardsSpec
import           GameSpec
import           Test.Hspec

main =
  hspec $ do
    cardTests
    gameTests
