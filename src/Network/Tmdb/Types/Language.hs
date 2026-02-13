{- | Language and locale types for TMDB API

TMDB uses language codes in the format @xx-XX@ where:

* @xx@ = ISO 639-1 language code (2 letters, lowercase)
* @XX@ = ISO 3166-1 country\/region code (2 letters, uppercase)

= Usage

@
import Network.Tmdb.Types.Language

-- Use predefined locales
config = TmdbConfig "api-key" zhCN

-- Or create custom locales
myLocale = TmdbLocale "ko-KR"  -- Korean (South Korea)
@
-}
module Network.Tmdb.Types.Language
  ( -- * Types
    TmdbLocale (..)

    -- * Common Locales
  , enUS
  , enGB
  , zhCN
  , zhTW
  , jaJP
  , koKR
  , frFR
  , deDE
  , esES
  , esMX
  , ptBR
  , ptPT
  , itIT
  , ruRU
  )
where

import Data.Text (Text)
import Web.HttpApiData (ToHttpApiData (..))

-- | TMDB locale in @xx-XX@ format (e.g., @"en-US"@, @"zh-CN"@)
--
-- Combines an ISO 639-1 language code with an ISO 3166-1 country code.
newtype TmdbLocale = TmdbLocale {unTmdbLocale :: Text}
  deriving stock (Show, Eq)
  deriving newtype (ToHttpApiData)

-- * Common Locales

-- | English (United States)
enUS :: TmdbLocale
enUS = TmdbLocale "en-US"

-- | English (United Kingdom)
enGB :: TmdbLocale
enGB = TmdbLocale "en-GB"

-- | Chinese (China)
zhCN :: TmdbLocale
zhCN = TmdbLocale "zh-CN"

-- | Chinese (Taiwan)
zhTW :: TmdbLocale
zhTW = TmdbLocale "zh-TW"

-- | Japanese (Japan)
jaJP :: TmdbLocale
jaJP = TmdbLocale "ja-JP"

-- | Korean (South Korea)
koKR :: TmdbLocale
koKR = TmdbLocale "ko-KR"

-- | French (France)
frFR :: TmdbLocale
frFR = TmdbLocale "fr-FR"

-- | German (Germany)
deDE :: TmdbLocale
deDE = TmdbLocale "de-DE"

-- | Spanish (Spain)
esES :: TmdbLocale
esES = TmdbLocale "es-ES"

-- | Spanish (Mexico)
esMX :: TmdbLocale
esMX = TmdbLocale "es-MX"

-- | Portuguese (Brazil)
ptBR :: TmdbLocale
ptBR = TmdbLocale "pt-BR"

-- | Portuguese (Portugal)
ptPT :: TmdbLocale
ptPT = TmdbLocale "pt-PT"

-- | Italian (Italy)
itIT :: TmdbLocale
itIT = TmdbLocale "it-IT"

-- | Russian (Russia)
ruRU :: TmdbLocale
ruRU = TmdbLocale "ru-RU"
