{-# LANGUAGE OverloadedStrings #-}

module Text.Mining.Stemming.English where

import Data.Text (Text)

stem :: Text -> Text
stem = undefined

vowels :: [Char]
vowels = ['a', 'e', 'i', 'o', 'u', 'y']
