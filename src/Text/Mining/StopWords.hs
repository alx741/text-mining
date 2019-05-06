-- | Remove the Stop Words of a text.
--
-- An assortment of stop words lexica can be found together with the code of
-- this library:
-- https://github.com/alx741/text-mining_haskell/tree/master/data/stopwords

module Text.Mining.StopWords
    (
    -- * Stop words removal
      StopWordsLexicon
    , removeStopWords
    , readLexiconFile
    , lexiconFromList

    -- * Stop words removal ignoring diacritics
    , StopWordsLexiconNoDiacritics
    , removeStopWordsIgnoreDiacritics
    , readLexiconFileIgnoreDiacritics
    , lexiconFromListIgnoreDiacritics
    )
    where

import Data.Set     (Set, fromList, member)
import Data.Text    as T (Text, map, toLower, unwords, words)
import Data.Text.IO (readFile)
import Prelude      hiding (readFile, unwords, words)


data StopWordsLexicon = StopWordsLexicon (Set Text)

-- | In some languages and in some scenarios it becomes useful to ignore
-- diacritics, as the source of the text might or might not contain them
-- properly. Spanish is such a language and ASR is such a scenario. For those
-- use a 'StopWordsLexiconNoDiacritics'.
data StopWordsLexiconNoDiacritics = StopWordsLexiconNoDiacritics (Set Text)


removeStopWords :: StopWordsLexicon -> Text -> Text
removeStopWords (StopWordsLexicon lexicon)
    = unwords
    . filter (\w -> not $ toLower w `member` lexicon)
    . words

removeStopWordsIgnoreDiacritics :: StopWordsLexiconNoDiacritics -> Text -> Text
removeStopWordsIgnoreDiacritics (StopWordsLexiconNoDiacritics lexicon)
    = unwords
    . filter (\w -> not $ (removeDiacritics . toLower) w `member` lexicon)
    . words

lexiconFromList :: [Text] -> StopWordsLexicon
lexiconFromList = StopWordsLexicon . fromList . fmap toLower

lexiconFromListIgnoreDiacritics :: [Text] -> StopWordsLexiconNoDiacritics
lexiconFromListIgnoreDiacritics
    = StopWordsLexiconNoDiacritics
    . fromList
    . fmap (removeDiacritics . toLower)

-- | Read a 'StopWordsLexicon' from a *one word per line* file
readLexiconFile :: FilePath -> IO StopWordsLexicon
readLexiconFile fp
    = lexiconFromList
    . words
    <$> readFile fp

-- | Read a 'StopWordsLexiconNoDiacritics' from a *one word per line* file
readLexiconFileIgnoreDiacritics :: FilePath -> IO StopWordsLexiconNoDiacritics
readLexiconFileIgnoreDiacritics fp
    = lexiconFromListIgnoreDiacritics
    . words
    <$> readFile fp

removeDiacritics :: Text -> Text
removeDiacritics = T.map unDiacritic
    where
        unDiacritic :: Char -> Char
        -- Acute accent
        unDiacritic 'Á' = 'A'
        unDiacritic 'É' = 'E'
        unDiacritic 'Í' = 'I'
        unDiacritic 'Ó' = 'O'
        unDiacritic 'Ú' = 'U'
        unDiacritic 'á' = 'a'
        unDiacritic 'é' = 'e'
        unDiacritic 'í' = 'i'
        unDiacritic 'ó' = 'o'
        unDiacritic 'ú' = 'u'

        -- Diaeresis
        unDiacritic 'Ä' = 'A'
        unDiacritic 'Ë' = 'E'
        unDiacritic 'Ï' = 'I'
        unDiacritic 'Ö' = 'O'
        unDiacritic 'Ü' = 'U'
        unDiacritic 'ä' = 'a'
        unDiacritic 'ë' = 'e'
        unDiacritic 'ï' = 'i'
        unDiacritic 'ö' = 'o'
        unDiacritic 'ü' = 'u'

        -- Tilde
        unDiacritic 'Ñ' = 'N'
        unDiacritic 'ñ' = 'n'

        unDiacritic x   = x
