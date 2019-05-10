{-# LANGUAGE OverloadedStrings #-}

module Text.Mining.Stemming.Common where

import Data.Bool (bool)
import Data.List (sortOn)
import Data.Ord  (Down (..))
import Data.Text as T (Text, dropEnd, isSuffixOf, length, pack, snoc, takeEnd,
                       unpack)

-- | Split a word on its R1 region
--
-- Defined in: http://snowball.tartarus.org/texts/r1r2.html
regionR1 :: [Char] -- ^ Lost of vowels
 -> Text -- ^ Word
 -> (Text, Text) -- ^ (Base, R1)
regionR1 vowels word = parseRegion (unpack word) ("", "")
    where
        parseRegion :: String -> (Text, Text) -> (Text, Text)
        parseRegion (x:y:xs) (b, r1)
            | isVowel x && (not . isVowel) y = (b `snoc` x `snoc` y, pack xs)
            | otherwise = parseRegion (y:xs) (b `snoc` x, r1)
        parseRegion [x] (b, r1) = (b `snoc` x, r1)
        parseRegion []  (b, r1) = (b, r1)

        isVowel = flip elem vowels


-- | Split a word on its R2 region
--
-- Defined in: http://snowball.tartarus.org/texts/r1r2.html
regionR2 :: [Char] -- ^ Lost of vowels
 -> Text -- ^ Word
 -> (Text, Text) -- ^ (Base, R2)
regionR2 vowels word =
    let (base1, r1) = regionR1 vowels word
        (base2, r2) = regionR1 vowels r1
    in (base1 <> base2, r2)


-- | Drop a /suffix/ from a /text/
-- If the /suffix/ is not found, the /text/ is left unchanged
dropSuffix :: Text -> Text -> Text
dropSuffix suffix t = bool t (dropEnd n t) (takeEnd n t == suffix)
    where n = T.length suffix

-- | Drop the longest /suffix/ in the list from a /text/
-- If none of the /suffixes/ is not found, the /text/ is left unchanged
dropLongestSuffix :: [Text] -> Text -> Text
dropLongestSuffix suffixes t =
    case longestSuffix suffixes t of
        Nothing     -> t
        Just suffix -> dropSuffix suffix t

-- | Just the longest suffix in a /text/
-- Nothing if none of the suffixes is found in the /text/
longestSuffix :: [Text] -> Text -> Maybe Text
longestSuffix = findSuffix . sortOn (Down . T.length)
    where
        findSuffix :: [Text] -> Text -> Maybe Text
        findSuffix [] _ = Nothing
        findSuffix (x:xs) t = bool (findSuffix xs t) (Just x) (x `isSuffixOf` t)
