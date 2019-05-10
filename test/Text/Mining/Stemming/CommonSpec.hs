{-# LANGUAGE OverloadedStrings #-}

module Text.Mining.Stemming.CommonSpec where

import Test.Hspec
import Text.Mining.Stemming.Common
import Text.Mining.Stemming.English (vowels)

spec :: Spec
spec = do
    describe "regionR1" $ do
        it "Takes the base and R1 region of a word" $ do
            regionR1 vowels "beautiful"     `shouldBe` ("beaut", "iful")
            regionR1 vowels "beauty"        `shouldBe` ("beaut", "y")
            regionR1 vowels "beau"          `shouldBe` ("beau",  "")
            regionR1 vowels "animadversion" `shouldBe` ("an",    "imadversion")
            regionR1 vowels "sprinkled"     `shouldBe` ("sprin", "kled")
            regionR1 vowels "eucharist"     `shouldBe` ("euc",   "harist")

    describe "regionR2" $ do
        it "Takes the base and R2 region of a word" $ do
            regionR2 vowels "beautiful"     `shouldBe` ("beautif",   "ul")
            regionR2 vowels "beauty"        `shouldBe` ("beauty",    "")
            regionR2 vowels "beau"          `shouldBe` ("beau",      "")
            regionR2 vowels "animadversion" `shouldBe` ("anim",      "adversion")
            regionR2 vowels "sprinkled"     `shouldBe` ("sprinkled", "")
            regionR2 vowels "eucharist"     `shouldBe` ("euchar",    "ist")

