module CardsSpec where

import           Data.Map    (toList)
import           Poker.Cards (groupBySuit, groupByValue, newDeck)
import           Test.Hspec

cardTests :: Spec
cardTests =
  describe "A fresh deck" $ do
    it "should have 52 cards" $ length newDeck `shouldBe` (52 :: Int)
    describe "Should be grouped correctly" $ do
      it "by value" $
        let groups = toList $ groupByValue newDeck
         in all ((== 4) . length . snd) groups `shouldBe` True
      it "by suit" $
        let groups = toList $ groupBySuit newDeck
         in all ((== 13) . length . snd) groups `shouldBe` True
