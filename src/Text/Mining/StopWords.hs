module Text.Mining.StopWords where

import Data.Set     (Set, fromList, member)
import Data.Text    (Text, toLower, unwords, words)
import Data.Text.IO (readFile)
import Prelude      hiding (readFile, unwords, words)

type StopWordsLexicon = Set Text

removeStopWords :: StopWordsLexicon -> Text -> Text
removeStopWords lexicon
    = unwords
    . filter (\w -> not $ toLower w `member` lexicon)
    . words

loadLexiconFile :: FilePath -> IO StopWordsLexicon
loadLexiconFile fp
    = fromList
    . fmap toLower
    . words
    <$> readFile fp
