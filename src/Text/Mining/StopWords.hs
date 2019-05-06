-- | Remove the Stop Words in a 'StopWordsLexicon' from a text corpus.
--
-- In some languages and in some scenarios it becomes useful to ignore
-- diacritics as the source of the text might or might not contain them properly
-- Spanish is such a language and ASR is such a scenario. For those use a
-- 'StopWordsLexiconNoDiacritics'.
--
-- An assortment of stop words lexica can be found together with the code of
-- this library: https://github.com/alx741/text-mining_haskell/tree/master/data

module Text.Mining.StopWords
    (
    -- * Stop words removal preserving diacritics
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

readLexiconFile :: FilePath -> IO StopWordsLexicon
readLexiconFile fp
    = StopWordsLexicon
    . fromList
    . fmap toLower
    . words
    <$> readFile fp

readLexiconFileIgnoreDiacritics :: FilePath -> IO StopWordsLexiconNoDiacritics
readLexiconFileIgnoreDiacritics fp
    = StopWordsLexiconNoDiacritics
    . fromList
    . fmap (removeDiacritics . toLower)
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
