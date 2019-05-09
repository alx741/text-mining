{-# LANGUAGE OverloadedStrings #-}

module Text.Mining.StemmingSpec where

import Test.Hspec
import Text.Mining.Stemming

spec :: Spec
spec = do
    describe "regionR1" $ do
        it "Takes the base and R1 region of a word" $ do
            regionR1 englishVowels "beautiful"     `shouldBe` ("beaut", "iful")
            regionR1 englishVowels "beauty"        `shouldBe` ("beaut", "y")
            regionR1 englishVowels "beau"          `shouldBe` ("beau",  "")
            regionR1 englishVowels "animadversion" `shouldBe` ("an",    "imadversion")
            regionR1 englishVowels "sprinkled"     `shouldBe` ("sprin", "kled")
            regionR1 englishVowels "eucharist"     `shouldBe` ("euc",   "harist")

    describe "regionR2" $ do
        it "Takes the base and R2 region of a word" $ do
            regionR2 englishVowels "beautiful"     `shouldBe` ("beautif",   "ul")
            regionR2 englishVowels "beauty"        `shouldBe` ("beauty",    "")
            regionR2 englishVowels "beau"          `shouldBe` ("beau",      "")
            regionR2 englishVowels "animadversion" `shouldBe` ("anim",      "adversion")
            regionR2 englishVowels "sprinkled"     `shouldBe` ("sprinkled", "")
            regionR2 englishVowels "eucharist"     `shouldBe` ("euchar",    "ist")

    describe "spanishStem" $ do
        it "takes the stem from Spanish words" $ do
            stem "cheque"     `shouldBe` "chequ"
            stem "chequeo"    `shouldBe` "cheque"
            stem "cheques"    `shouldBe` "chequ"
            stem "chica"      `shouldBe` "chic"
            stem "torcer"     `shouldBe` "torc"
            stem "torear"     `shouldBe` "tor"
            stem "tormenta"   `shouldBe` "torment"
            stem "tormentas"  `shouldBe` "torment"
            stem "torre"      `shouldBe` "torr"
            stem "torrencial" `shouldBe` "torrencial"
