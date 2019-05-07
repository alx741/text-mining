{-# LANGUAGE OverloadedStrings #-}

module Text.Mining.Stemming where

import Data.Text (Text, pack, unpack)

spanishStem :: Text -> Text
spanishStem = undefined

-- | Take the regionns /(R1, R2)/ of a word.
--
-- Defined in: http://snowball.tartarus.org/texts/r1r2.html
regions :: [Char] -- ^ List of vowels
 -> Text -- ^ Word
 -> (Text, Text) -- ^ Regions (R1, R2)
regions vowels t = (region1 vowels t, region2 vowels t)

region1 :: [Char] -- ^ List of vowels
 -> Text -- ^ Word
 -> Text -- ^ Region
region1 vowels = pack . parseRegion vowels . unpack
    where
        parseRegion :: [Char] -> String -> String
        parseRegion vowels (x:y:xs)
            | x `elem` vowels && not (y `elem` vowels) = xs
            | otherwise = parseRegion vowels (y:xs)
        parseRegion vowels _ = ""

region2 :: [Char] -- ^ List of vowels
 -> Text -- ^ R1 (region1 of a word)
 -> Text -- ^ Region
region2 vowels = region1 vowels . region1 vowels

spanishVowels :: [Char]
spanishVowels = ['a', 'e', 'i', 'o', 'u', 'á', 'é', 'í', 'ó', 'ú', 'ü']

englishVowels :: [Char]
englishVowels = ['a', 'e', 'i', 'o', 'u', 'y']
