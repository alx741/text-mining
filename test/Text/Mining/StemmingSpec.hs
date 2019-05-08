{-# LANGUAGE OverloadedStrings #-}

module Text.Mining.StemmingSpec where

import Test.Hspec
import Text.Mining.Stemming

spec :: Spec
spec = do
    describe "regions" $ do
        it "Takes the stemming regions of a word" $ do
            regions englishVowels "beautiful"     `shouldBe` ("iful",        "ul")
            regions englishVowels "beauty"        `shouldBe` ("y",           "")
            regions englishVowels "beau"          `shouldBe` ("",            "")
            regions englishVowels "animadversion" `shouldBe` ("imadversion", "adversion")
            regions englishVowels "sprinkled"     `shouldBe` ("kled",        "")
            regions englishVowels "eucharist"     `shouldBe` ("harist",      "ist")

    describe "spanishStem" $ do
        it "takes the stem from Spanish words" $ do
            spanishStem "cheque"     `shouldBe` "chequ"
            spanishStem "chequeo"    `shouldBe` "cheque"
            spanishStem "cheques"    `shouldBe` "chequ"
            spanishStem "chica"      `shouldBe` "chic"
            spanishStem "torcer"     `shouldBe` "torc"
            spanishStem "torear"     `shouldBe` "tor"
            spanishStem "tormenta"   `shouldBe` "torment"
            spanishStem "tormentas"  `shouldBe` "torment"
            spanishStem "torre"      `shouldBe` "torr"
            spanishStem "torrencial" `shouldBe` "torrencial"
