{-# LANGUAGE OverloadedStrings #-}

module Text.Mining.Stemming where

import Data.Maybe (mapMaybe)
import Data.Text  as T (Text, breakOn, cons, last, pack, span, splitAt,
                        stripSuffix, unpack, init)
import Safe       (headMay)

import Text.Mining.Diacritics (removeDiacritics)

spanishStem :: Text -> Text
spanishStem = undefined

removeAttachedPronoun :: Text -> Text
removeAttachedPronoun t =
    case matchPrefix rv of
        Nothing -> t
        Just (start, prefix) ->
            case stripSuffixes prefix of
                Nothing  -> base <> start <> prefix
                Just end -> base <> start <> removeDiacritics end
    where
    -- Match prefix in RV + one preceding character in order to match the case
    -- of "yendo" following 'u' that could be right before RV
    (base, rv) = let (base', rv') = regionRV t
        in (T.init base', T.last base' `cons` rv')

    matchPrefix t = headMay
        $ filter (\(_, prefix) -> prefix /= "")
        $ fmap (`breakOn` t) prefixes

    stripSuffixes t = headMay $ mapMaybe (`stripSuffix` t) suffixes

    prefixes =
        [ "iéndo", "ándo", "ár", "ér", "ír"
        , "iendo", "ando", "ar", "er", "ir", "uyendo"]

    suffixes =
        [ "me", "se", "sela", "selo", "selas", "selos"
        , "la", "le", "lo", "las", "les", "los", "nos"]


regionRV :: Text -> (Text, Text)
regionRV = takeRV . unpack
    where
        takeRV :: String -> (Text, Text)
        takeRV [x,y] = (pack $ x:[y], "")
        takeRV word@(x:y:xs)
            | not (isVowel y) =
                let (prefix, region) = T.span isVowel $ pack xs
                in (x `cons` y `cons` prefix, region)
            | isVowel x && isVowel y =
                let (prefix, region) = T.span (not . isVowel) $ pack xs
                in (x `cons` y `cons` prefix, region)
            | otherwise = T.splitAt 3 $ pack word
        takeRV word = (pack word, "")

        isVowel :: Char -> Bool
        isVowel = flip elem spanishVowels

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
        parseRegion vs (x:y:xs)
            | x `elem` vs && not (y `elem` vs) = xs
            | otherwise = parseRegion vs (y:xs)
        parseRegion _ _ = ""

region2 :: [Char] -- ^ List of vowels
 -> Text -- ^ R1 (region1 of a word)
 -> Text -- ^ Region
region2 vowels = region1 vowels . region1 vowels

spanishVowels :: [Char]
spanishVowels = ['a', 'e', 'i', 'o', 'u', 'á', 'é', 'í', 'ó', 'ú', 'ü']

englishVowels :: [Char]
englishVowels = ['a', 'e', 'i', 'o', 'u', 'y']
