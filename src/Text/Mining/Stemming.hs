{-# LANGUAGE OverloadedStrings #-}

module Text.Mining.Stemming where

import Data.Bool  (bool)
import Data.List  (sortOn)
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Ord   (Down (..))
import Data.Text  as T (Text, breakOn, cons, init, isSuffixOf, last, length,
                        pack, span, splitAt, stripSuffix, unpack)
import Safe       (headMay)

import Text.Mining.Diacritics (removeDiacritics)

spanishStem :: Text -> Text
spanishStem = undefined

removeStandardSuffix :: Text -> Text
removeStandardSuffix t =
    case longestSuffix allSuffixes r2 of
        Nothing -> t
        Just suffix
            | suffix `elem` suffixesCase1 -> base <> (fromMaybe r2 $ stripSuffix suffix r2)
            | suffix `elem` suffixesCase2 -> undefined
            | suffix `elem` suffixesCase3 -> undefined
            | suffix `elem` suffixesCase4 -> undefined
            | suffix `elem` suffixesCase5 -> undefined
            | suffix `elem` suffixesCase6 -> undefined
            | suffix `elem` suffixesCase7 -> undefined
            | suffix `elem` suffixesCase8 -> undefined
            | suffix `elem` suffixesCase9 -> undefined
    where
        (base, r2) = undefined

        allSuffixes
            =  suffixesCase1 <> suffixesCase2 <> suffixesCase3
            <> suffixesCase4 <> suffixesCase5 <> suffixesCase6
            <> suffixesCase7 <> suffixesCase8 <> suffixesCase9

        suffixesCase1 = -- Remove if in R2
            [ "anza", "anzas", "ico", "ica", "icos", "icas", "ismo", "able"
            , "ables", "ible", "ibles", "ista", "istas", "oso", "osa", "osos"
            , "osas", "amiento", "amientos", "imiento", "imientos"
            ]

        suffixesCase2 = -- Remove if in R2, remove "ic" prefix
            [ "adora", "ador", "ación", "adoras", "adores"
            , "aciones", "ante", "antes", "ancia", "ancias"
            ]

        suffixesCase3 = -- Replace with "log" if in R2
            ["logía", "logías"]

        suffixesCase4 = -- Replace with "u" if in R2
            ["ución", "uciones"]

        suffixesCase5 = -- Replace with "ente" if in R2
            ["encia", "encias"]

        suffixesCase6 =
            -- Remove if in R2
            --   remove "iv" or "ativ" prefix
            --   remove "os", "ic", "ad" prefixes
            ["amente"]

        suffixesCase7 =
            -- Remove if in R2
            --   remove "ante", "able", "ible" prefixes
            ["mente"]

        suffixesCase8 =
            -- Remove if in R2
            --   remove "abil", "ic", "iv" prefixes
            ["idad", "idades"]

        suffixesCase9 = -- Remove if in R2, remove "at" prefix
            ["iva", "ivo", "ivas", "ivos"]


-- FIXME: Use longestSuffix
removeAttachedPronoun :: Text -> Text
removeAttachedPronoun t =
    case matchPrefix rv of
        Nothing -> t
        Just (start, prefix) ->
            case stripSuffixes prefix of
                Nothing  -> base <> start <> prefix
                Just end -> base <> start <> removeDiacritics end
    where
    (base, rv) = regionRV t

    matchPrefix t = headMay
        $ filter (\(_, prefix) -> prefix /= "")
        $ fmap (`breakOn` t) prefixes

    stripSuffixes t = headMay $ mapMaybe (`stripSuffix` t) suffixes

    prefixes =
        [ "iéndo", "ándo", "ár", "ér", "ír"
        , "iendo", "ando", "ar", "er", "ir", "uyendo"
        ]

    suffixes =
        [ "me", "se", "sela", "selo", "selas", "selos"
        , "la", "le", "lo", "las", "les", "los", "nos"
        ]


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
        isVowel = flip elem
            ['a', 'e', 'i', 'o', 'u', 'á', 'é', 'í', 'ó', 'ú', 'ü']

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

englishVowels :: [Char]
englishVowels = ['a', 'e', 'i', 'o', 'u', 'y']

longestSuffix :: [Text] -> Text -> Maybe Text
longestSuffix = findSuffix . sortOn (Down . T.length)
    where
        findSuffix :: [Text] -> Text -> Maybe Text
        findSuffix [] _ = Nothing
        findSuffix (x:xs) t = bool (findSuffix xs t) (Just x) (x `isSuffixOf` t)
