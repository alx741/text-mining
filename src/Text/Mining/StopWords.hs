module Text.Mining.StopWords where

import Data.Set     (Set, fromList)
import Data.Text    (Text, toLower, words)
import Data.Text.IO (readFile)
import Prelude      hiding (readFile, words)

type StopWordsLexicon = Set Text

loadLexiconFile :: FilePath -> IO StopWordsLexicon
loadLexiconFile fp
    = fromList
    . fmap toLower
    . words
    <$> readFile fp
