{-# LANGUAGE OverloadedStrings #-}

module Text.Mining.Stemming where

import Data.Bool  (bool)
import Data.List  (sortOn)
import Data.Maybe (mapMaybe)
import Data.Ord   (Down (..))
import Data.Text  as T (Text, breakOn, cons, dropEnd, head, isSuffixOf, length,
                        pack, replace, snoc, span, splitAt, stripSuffix,
                        takeEnd, unpack)
import Safe       (headMay)

import Text.Mining.Diacritics (removeAcuteAccents, removeAllDiacritics)

stem :: Text -> Text
stem = removeAcuteAccents
    . removeResidualSuffix
    . removeStandardSuffix
    . removeAttachedPronoun


-- FIXME: Use longestSuffix
-- | Step 0: Attached pronoun
removeAttachedPronoun :: Text -> Text
removeAttachedPronoun t =
    case matchPrefix rv of
        Nothing -> t
        Just (start, prefix) ->
            case stripSuffixes prefix of
                Nothing  -> base <> start <> prefix
                Just end -> base <> start <> removeAllDiacritics end
    where
    (base, rv) = regionRV t

    matchPrefix t' = headMay
        $ filter (\(_, prefix) -> prefix /= "")
        $ fmap (`breakOn` t') prefixes

    stripSuffixes t' = headMay $ mapMaybe (`stripSuffix` t') suffixes

    prefixes =
        [ "iéndo", "ándo", "ár", "ér", "ír"
        , "iendo", "ando", "ar", "er", "ir", "uyendo"
        ]

    suffixes =
        [ "me", "se", "sela", "selo", "selas", "selos"
        , "la", "le", "lo", "las", "les", "los", "nos"
        ]


-- | Step 1: Standard suffix removal
removeStandardSuffix :: Text -> Text
removeStandardSuffix t =
    case longestSuffix allSuffixes r2 of
        Nothing -> removeYVerbSuffixes t
        Just suffix
            | suffix `elem` suffixesCase1 -> base <> dropSuffix suffix r2
            | suffix `elem` suffixesCase2 -> dropSuffix "ic" $ base <> dropSuffix suffix r2
            | suffix `elem` suffixesCase3 -> base <> replace suffix "log"  r2
            | suffix `elem` suffixesCase4 -> base <> replace suffix "u"    r2
            | suffix `elem` suffixesCase5 -> base <> replace suffix "ente" r2
            | suffix `elem` suffixesCase6 ->
                let stripped = base <> dropSuffix suffix r2
                in dropLongestSuffix ["ativ", "iv", "os", "ic", "ad"] stripped
            | suffix `elem` suffixesCase7 ->
                let stripped = base <> dropSuffix suffix r2
                in dropLongestSuffix ["ante", "able", "ible"] stripped
            | suffix `elem` suffixesCase8 ->
                let stripped = base <> dropSuffix suffix r2
                in dropLongestSuffix ["abil", "ic", "iv"] stripped
            | suffix `elem` suffixesCase9 -> dropSuffix "at" $ base <> dropSuffix suffix r2
            | otherwise -> removeYVerbSuffixes t
    where
        (base, r2) = regionR2 spanishVowels t

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


-- | Step 2a: Verb suffixes beginning with 'y'
removeYVerbSuffixes :: Text -> Text
removeYVerbSuffixes t =
    case longestSuffix suffixes rv of
        Nothing -> removeOtherVerbSuffixes t
        Just suffix ->
            let stripped = base <> dropSuffix suffix rv
            in bool (removeOtherVerbSuffixes t) stripped ("u" `isSuffixOf` stripped)
    where
        (base, rv) = regionRV t

        suffixes = -- Remove if in RV and preceded by 'u'
            [ "ya", "ye", "yan", "yen", "yeron", "yendo"
            , "yo", "yó", "yas", "yes", "yais", "yamos"
            ]


-- | Step 2b: Verb suffixes beginning with 'y'
removeOtherVerbSuffixes :: Text -> Text
removeOtherVerbSuffixes t =
    case longestSuffix allSuffixes rv of
        Nothing -> t
        Just suffix
            | suffix `elem` suffixesCase1 ->
                let stripped = base <> dropSuffix suffix rv
                in bool
                    stripped
                    (dropSuffix "u" stripped)
                    ("gu" `isSuffixOf` stripped)
            | suffix `elem` suffixesCase2 -> base <> dropSuffix suffix rv
            | otherwise -> t
    where
        (base, rv) = regionRV t

        allSuffixes = suffixesCase1 <> suffixesCase2

        suffixesCase1 = -- Remove if in RV, remove 'u' if preceded by "gu"
            ["en", "es", "éis", "emos"]

        suffixesCase2 = -- Remove if in RV
            [ "arían", "arías", "arán", "arás", "aríais", "aría", "aréis"
            , "aríamos", "aremos", "ará", "aré", "erían", "erías", "erán"
            , "erás", "eríais", "ería", "eréis", "eríamos", "eremos", "erá"
            , "eré", "irían", "irías", "irán", "irás", "iríais", "iría"
            , "iréis", "iríamos", "iremos", "irá", "iré", "aba", "ada", "ida"
            , "ía", "ara", "iera", "ad", "ed", "id", "ase", "iese", "aste"
            , "iste", "an", "aban", "ían", "aran", "ieran", "asen", "iesen"
            , "aron", "ieron", "ado", "ido", "ando", "iendo", "ió", "ar", "er"
            , "ir", "as", "abas", "adas", "idas", "ías", "aras", "ieras"
            , "ases", "ieses", "ís", "áis", "abais", "íais", "arais", "ierais"
            , "aseis", "ieseis", "asteis", "isteis", "ados", "idos", "amos"
            , "ábamos", "íamos", "imos", "áramos", "iéramos", "iésemos", "ásemos"
            ]


-- | Step 3: Residual suffix
removeResidualSuffix :: Text -> Text
removeResidualSuffix t =
    case longestSuffix allSuffixes rv of
        Nothing -> t
        Just suffix
            | suffix `elem` suffixesCase1 -> base <> dropSuffix suffix rv
            | suffix `elem` suffixesCase2 ->
                let stripped = base <> dropSuffix suffix rv
                in bool
                    stripped
                    (dropSuffix "gu" stripped)
                    ("gu" `isSuffixOf` stripped && T.head rv == 'u')
            | otherwise -> t
    where
        (base, rv) = regionRV t

        allSuffixes = suffixesCase1 <> suffixesCase2

        suffixesCase1 = -- Remove if in RV
            ["os", "a", "o", "á", "í", "ó"]

        suffixesCase2 = -- Remove if in RV, if prefixed by "gu" remove "u" in RV
            ["e", "é"]



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


englishVowels :: [Char]
englishVowels = ['a', 'e', 'i', 'o', 'u', 'y']

spanishVowels :: [Char]
spanishVowels = ['a', 'e', 'i', 'o', 'u', 'á', 'é', 'í', 'ó', 'ú', 'ü']

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
