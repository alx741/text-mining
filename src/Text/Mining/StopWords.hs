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

type StopWordsLexicon = Set Text
type StopWordsLexiconNoDiacritics = Set Text

removeStopWords :: StopWordsLexicon -> Text -> Text
removeStopWords lexicon
    = unwords
    . filter (\w -> not $ toLower w `member` lexicon)
    . words

removeStopWordsIgnoreDiacritics :: StopWordsLexiconNoDiacritics -> Text -> Text
removeStopWordsIgnoreDiacritics lexicon
    = unwords
    . filter (\w -> not $ (removeDiacritics . toLower) w `member` lexicon)
    . words

lexiconFromList :: [Text] -> StopWordsLexicon
lexiconFromList = fromList . fmap toLower

lexiconFromListIgnoreDiacritics :: [Text] -> StopWordsLexiconNoDiacritics
lexiconFromListIgnoreDiacritics = fromList . fmap (removeDiacritics . toLower)

readLexiconFile :: FilePath -> IO StopWordsLexicon
readLexiconFile fp
    = fromList
    . fmap toLower
    . words
    <$> readFile fp

readLexiconFileIgnoreDiacritics :: FilePath -> IO StopWordsLexiconNoDiacritics
readLexiconFileIgnoreDiacritics fp
    = fromList
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
