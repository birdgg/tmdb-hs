module Test.Tmdb.Types.LanguageSpec (spec) where

import qualified Country.Identifier as Country
import Network.Tmdb.Types.Language
import Test.Hspec
import Web.HttpApiData (toQueryParam)

spec :: Spec
spec = describe "TmdbLocale" $ do
  describe "ToHttpApiData" $ do
    it "serializes enUS correctly" $ do
      toQueryParam enUS `shouldBe` "en-US"

    it "serializes zhCN correctly" $ do
      toQueryParam zhCN `shouldBe` "zh-CN"

    it "serializes zhTW correctly" $ do
      toQueryParam zhTW `shouldBe` "zh-TW"

    it "serializes jaJP correctly" $ do
      toQueryParam jaJP `shouldBe` "ja-JP"

    it "serializes koKR correctly" $ do
      toQueryParam koKR `shouldBe` "ko-KR"

    it "serializes enGB correctly" $ do
      toQueryParam enGB `shouldBe` "en-GB"

    it "serializes frFR correctly" $ do
      toQueryParam frFR `shouldBe` "fr-FR"

    it "serializes deDE correctly" $ do
      toQueryParam deDE `shouldBe` "de-DE"

    it "serializes esES correctly" $ do
      toQueryParam esES `shouldBe` "es-ES"

    it "serializes esMX correctly" $ do
      toQueryParam esMX `shouldBe` "es-MX"

    it "serializes ptBR correctly" $ do
      toQueryParam ptBR `shouldBe` "pt-BR"

    it "serializes ptPT correctly" $ do
      toQueryParam ptPT `shouldBe` "pt-PT"

    it "serializes itIT correctly" $ do
      toQueryParam itIT `shouldBe` "it-IT"

    it "serializes ruRU correctly" $ do
      toQueryParam ruRU `shouldBe` "ru-RU"

  describe "mkLocale" $ do
    it "creates locale with correct language and country" $ do
      let locale = mkLocale "th" Country.thailand
      toQueryParam locale `shouldBe` "th-TH"

    it "lowercases language code" $ do
      let locale = mkLocale "EN" Country.unitedStatesOfAmerica
      toQueryParam locale `shouldBe` "en-US"

    it "handles mixed case language code" $ do
      let locale = mkLocale "Ja" Country.japan
      toQueryParam locale `shouldBe` "ja-JP"

  describe "Eq instance" $ do
    it "considers same locales equal" $ do
      enUS `shouldBe` mkLocale "en" Country.unitedStatesOfAmerica

    it "considers different locales not equal" $ do
      enUS `shouldNotBe` enGB

  describe "Show instance" $ do
    it "shows locale correctly" $ do
      show enUS `shouldContain` "TmdbLocale"
      show enUS `shouldContain` "en"
