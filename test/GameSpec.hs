module GameSpec where

import           Data.Map     (toList)
import           Poker.Config
import           Poker.Game
import           Test.Hspec

gameTests :: Spec
gameTests =
  describe "A fresh game" $ do
    it "should have 52 cards." $
      let game = newGame $ Config 2 42
       in length (deck game) `shouldBe` 52
    it "can be drawn from." $
      let game = newGame $ Config 2 42
          game' = dealCards 2 game
       in length (deck game') `shouldBe` 48
