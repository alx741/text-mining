module Text.Mining.Diacritics where

import Data.Text as T (Text, map)

removeAllDiacritics :: Text -> Text
removeAllDiacritics
    = removeTildes
    . removeDiaeresis
    . removeAcuteAccents

removeAcuteAccents :: Text -> Text
removeAcuteAccents = T.map unDiacritic
    where
        unDiacritic :: Char -> Char
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
        unDiacritic x   = x

removeDiaeresis :: Text -> Text
removeDiaeresis = T.map unDiacritic
    where
        unDiacritic :: Char -> Char
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
        unDiacritic x   = x

removeTildes :: Text -> Text
removeTildes = T.map unDiacritic
    where
        unDiacritic :: Char -> Char
        unDiacritic 'Ñ' = 'N'
        unDiacritic 'ñ' = 'n'
        unDiacritic x   = x
