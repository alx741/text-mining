-- | Remove stop words in a text.
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
import Data.Text    (Text, strip, toLower, unwords, words)
import Data.Text.IO (readFile)
import Prelude      hiding (readFile, unwords, words)

import Text.Mining.Diacritics (removeDiacritics)

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
lexiconFromList = StopWordsLexicon . fromList . fmap (strip . toLower)

lexiconFromListIgnoreDiacritics :: [Text] -> StopWordsLexiconNoDiacritics
lexiconFromListIgnoreDiacritics
    = StopWordsLexiconNoDiacritics
    . fromList
    . fmap (removeDiacritics . strip . toLower)

-- | Read a 'StopWordsLexicon' from a /one word per line/ file
readLexiconFile :: FilePath -> IO StopWordsLexicon
readLexiconFile fp
    = lexiconFromList
    . words
    <$> readFile fp

-- | Read a 'StopWordsLexiconNoDiacritics' from a /one word per line/ file
readLexiconFileIgnoreDiacritics :: FilePath -> IO StopWordsLexiconNoDiacritics
readLexiconFileIgnoreDiacritics fp
    = lexiconFromListIgnoreDiacritics
    . words
    <$> readFile fp
