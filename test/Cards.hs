module Cards where

import Test.Hspec
import Poker.Cards (newDeck)

cardTests = 
    describe "A fresh deck" $
        it "should have 52 cards" $
            length newDeck `shouldBe` (52 :: Int)
