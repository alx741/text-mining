{-# LANGUAGE OverloadedStrings #-}

module Text.Mining.StemmingSpec where

import Test.Hspec
import Text.Mining.Stemming

spec :: Spec
spec = do
    describe "regionR1" $ do
        it "Takes the base and R1 region of a word" $ do
            regionR1 englishVowels "beautiful"     `shouldBe` ("beaut", "iful")
            regionR1 englishVowels "beauty"        `shouldBe` ("beaut", "y")
            regionR1 englishVowels "beau"          `shouldBe` ("beau",  "")
            regionR1 englishVowels "animadversion" `shouldBe` ("an",    "imadversion")
            regionR1 englishVowels "sprinkled"     `shouldBe` ("sprin", "kled")
            regionR1 englishVowels "eucharist"     `shouldBe` ("euc",   "harist")

    describe "regionR2" $ do
        it "Takes the base and R2 region of a word" $ do
            regionR2 englishVowels "beautiful"     `shouldBe` ("beautif",   "ul")
            regionR2 englishVowels "beauty"        `shouldBe` ("beauty",    "")
            regionR2 englishVowels "beau"          `shouldBe` ("beau",      "")
            regionR2 englishVowels "animadversion" `shouldBe` ("anim",      "adversion")
            regionR2 englishVowels "sprinkled"     `shouldBe` ("sprinkled", "")
            regionR2 englishVowels "eucharist"     `shouldBe` ("euchar",    "ist")

    describe "spanishStem" $ do
        it "takes the stem from Spanish words" $ do
            stem "che"            `shouldBe` "che"
            stem "checa"          `shouldBe` "chec"
            stem "checar"         `shouldBe` "chec"
            stem "checo"          `shouldBe` "chec"
            stem "checoslovaquia" `shouldBe` "checoslovaqui"
            stem "chedraoui"      `shouldBe` "chedraoui"
            stem "chefs"          `shouldBe` "chefs"
            stem "cheliabinsk"    `shouldBe` "cheliabinsk"
            stem "chelo"          `shouldBe` "chel"
            stem "chemical"       `shouldBe` "chemical"
            stem "chemicalweek"   `shouldBe` "chemicalweek"
            stem "chemise"        `shouldBe` "chemis"
            stem "chepo"          `shouldBe` "chep"
            stem "cheque"         `shouldBe` "chequ"
            stem "chequeo"        `shouldBe` "cheque"
            stem "cheques"        `shouldBe` "chequ"
            stem "cheraw"         `shouldBe` "cheraw"
            stem "chesca"         `shouldBe` "chesc"
            stem "chester"        `shouldBe` "chest"
            stem "chetumal"       `shouldBe` "chetumal"
            stem "chetumaleños"   `shouldBe` "chetumaleñ"
            stem "chevrolet"      `shouldBe` "chevrolet"
            stem "cheyene"        `shouldBe` "cheyen"
            stem "cheyenne"       `shouldBe` "cheyenn"
            stem "chi"            `shouldBe` "chi"
            stem "chía"           `shouldBe` "chi"
            stem "chiapaneca"     `shouldBe` "chiapanec"
            stem "chiapas"        `shouldBe` "chiap"
            stem "chiba"          `shouldBe` "chib"
            stem "chic"           `shouldBe` "chic"
            stem "chica"          `shouldBe` "chic"
            stem "chicago"        `shouldBe` "chicag"
            stem "chicana"        `shouldBe` "chican"
            stem "chicano"        `shouldBe` "chican"
            stem "chicas"         `shouldBe` "chic"
            stem "chicharrones"   `shouldBe` "chicharron"
            stem "chichen"        `shouldBe` "chich"
            stem "chichimecas"    `shouldBe` "chichimec"
            stem "chicles"        `shouldBe` "chicl"
            stem "chico"          `shouldBe` "chic"
            stem "torá"           `shouldBe` "tor"
            stem "tórax"          `shouldBe` "torax"
            stem "torcer"         `shouldBe` "torc"
            stem "toreado"        `shouldBe` "tor"
            stem "toreados"       `shouldBe` "tor"
            stem "toreándolo"     `shouldBe` "tor"
            stem "torear"         `shouldBe` "tor"
            stem "toreara"        `shouldBe` "tor"
            stem "torearlo"       `shouldBe` "tor"
            stem "toreó"          `shouldBe` "tore"
            stem "torero"         `shouldBe` "torer"
            stem "toreros"        `shouldBe` "torer"
            stem "torio"          `shouldBe` "tori"
            stem "tormenta"       `shouldBe` "torment"
            stem "tormentas"      `shouldBe` "torment"
            stem "tornado"        `shouldBe` "torn"
            stem "tornados"       `shouldBe` "torn"
            stem "tornar"         `shouldBe` "torn"
            stem "tornen"         `shouldBe` "torn"
            stem "torneo"         `shouldBe` "torne"
            stem "torneos"        `shouldBe` "torne"
            stem "tornillo"       `shouldBe` "tornill"
            stem "tornillos"      `shouldBe` "tornill"
            stem "torniquete"     `shouldBe` "torniquet"
            stem "torno"          `shouldBe` "torn"
            stem "toro"           `shouldBe` "tor"
            stem "toronto"        `shouldBe` "toront"
            stem "toros"          `shouldBe` "tor"
            stem "torpedearon"    `shouldBe` "torped"
            stem "torpeza"        `shouldBe` "torpez"
            stem "torrado"        `shouldBe` "torr"
            stem "torralba"       `shouldBe` "torralb"
            stem "torre"          `shouldBe` "torr"
            stem "torrencial"     `shouldBe` "torrencial"
            stem "torrenciales"   `shouldBe` "torrencial"
            stem "torrente"       `shouldBe` "torrent"
            stem "torreon"        `shouldBe` "torreon"
            stem "torreón"        `shouldBe` "torreon"
            stem "torres"         `shouldBe` "torr"
            stem "torrescano"     `shouldBe` "torrescan"
