{-# LANGUAGE OverloadedStrings #-}

module Text.Mining.Stemming.English where

import Data.Text (Text)

-- | Take the (porter2) stem of an English word
--
-- Defined in: http://snowball.tartarus.org/algorithms/english/stemmer.html
stem :: Text -> Text
stem = undefined

-- | English vowels
vowels :: [Char]
vowels = ['a', 'e', 'i', 'o', 'u', 'y']
