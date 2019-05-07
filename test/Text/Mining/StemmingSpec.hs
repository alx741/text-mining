{-# LANGUAGE OverloadedStrings #-}

module Text.Mining.StemmingSpec where

import Test.Hspec
import Text.Mining.Stemming

spec :: Spec
spec = do
    describe "regions" $ do
        it "Takes the stemming regions of a word" $ do
            regions englishVowels "beautiful"  `shouldBe` ("iful", "ul")
            regions englishVowels "beauty"  `shouldBe` ("y", "")
            regions englishVowels "beau"  `shouldBe` ("", "")
            regions englishVowels "animadversion"  `shouldBe` ("imadversion", "adversion")
            regions englishVowels "sprinkled"  `shouldBe` ("kled", "")
            regions englishVowels "eucharist"  `shouldBe` ("harist", "ist")
