{-# LANGUAGE OverloadedStrings #-}

module Text.Mining.StopWordsSpec where

import Test.Hspec
import Text.Mining.StopWords

spec :: Spec
spec = do
    describe "removeStopWords" $ do
        it "Removes stop words that case match" $ do
            removeStopWords trivialEnglishLexicon  "I am here" `shouldBe` "here"
            removeStopWords trivialSpanishLexicon  "Eso está ahí" `shouldBe` "Eso ahí"
        it "Removes stop words that doesn't case match" $ do
            removeStopWords trivialEnglishLexicon  "i Am here" `shouldBe` "here"
            removeStopWords trivialSpanishLexicon  "Eso ESTÁ ahí" `shouldBe` "Eso ahí"
        it "Matches diacritics" $ do
            removeStopWords trivialSpanishLexicon  "Eso esta ahí" `shouldBe` "Eso esta ahí"

    describe "removeStopWordsIgnoreDiacritics" $ do
        it "Removes stop words regardless of diacritics" $ do
            removeStopWords trivialSpanishLexicon  "Eso está ahí" `shouldBe` "Eso ahí"
        it "Removes stop words regardless of diacritics and casing" $ do
            removeStopWords trivialSpanishLexicon  "Eso ESTÁ ahí" `shouldBe` "Eso ahí"


trivialEnglishLexicon :: StopWordsLexicon
trivialEnglishLexicon = lexiconFromList
    [ "a" , "am" , "and" , "are" , "as" , "at"
    , "but" , "by" , "is" , "I", "can", "do"
    ]

trivialSpanishLexicon :: StopWordsLexicon
trivialSpanishLexicon = lexiconFromList
    [ "un" , "soy" , "y" , "está" , "como" , "en"
    , "pero" , "por" , "es" , "Yo", "poder", "hacer"
    ]

spanishLexiconNoDiacritics :: StopWordsLexiconNoDiacritics
spanishLexiconNoDiacritics = lexiconFromListIgnoreDiacritics
    [ "un" , "soy" , "y" , "está" , "como" , "en"
    , "pero" , "por" , "es" , "Yo", "poder", "hacer"
    ]
