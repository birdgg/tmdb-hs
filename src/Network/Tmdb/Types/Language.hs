{- | Language and locale types for TMDB API

TMDB uses language codes in the format @xx-XX@ where:

* @xx@ = ISO 639-1 language code (2 letters, lowercase)
* @XX@ = ISO 3166-1 country/region code (2 letters, uppercase)

This module provides type-safe handling of these locale codes using
the @country@ package for region codes.

= Usage

@
import Network.Tmdb.Types.Language

-- Use predefined locales
config = TmdbConfig "api-key" zhCN

-- Or create custom locales
myLocale = mkLocale "ko" southKorea  -- Korean (South Korea)
@
-}
module Network.Tmdb.Types.Language
  ( -- * Types
    TmdbLocale (..)

    -- * Construction
  , mkLocale

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

    -- * Re-exports from Country
  , Country
  , Country.unitedStatesOfAmerica
  , Country.unitedKingdomOfGreatBritainAndNorthernIreland
  , Country.china
  , Country.taiwanProvinceOfChina
  , Country.japan
  , Country.koreaRepublicOf
  , Country.france
  , Country.germany
  , Country.spain
  , Country.mexico
  , Country.brazil
  , Country.portugal
  , Country.italy
  , Country.russianFederation
  )
where

import Country (Country)
import qualified Country
import qualified Country.Identifier as Country
import Data.Text (Text)
import qualified Data.Text as T
import Web.HttpApiData (ToHttpApiData (..))

-- | TMDB locale combining ISO 639-1 language code and ISO 3166-1 country code
--
-- The language code is a 2-letter lowercase ISO 639-1 code (e.g., "en", "zh", "ja").
-- The country/region is represented using the @Country@ type from the @country@ package.
data TmdbLocale = TmdbLocale
  { language :: Text
  -- ^ ISO 639-1 language code (2 letters, lowercase)
  , region :: Country
  -- ^ ISO 3166-1 country/region
  }
  deriving stock (Show, Eq)

-- | Create a locale from language code and country
--
-- @
-- mkLocale "en" unitedStatesOfAmerica  -- en-US
-- mkLocale "zh" china                   -- zh-CN
-- @
mkLocale :: Text -> Country -> TmdbLocale
mkLocale lang = TmdbLocale (T.toLower lang)

-- | Serialize locale to TMDB API format (e.g., "en-US", "zh-CN")
instance ToHttpApiData TmdbLocale where
  toQueryParam (TmdbLocale lang country) =
    lang <> "-" <> Country.alphaTwoUpper country

-- * Common Locales

-- | English (United States)
enUS :: TmdbLocale
enUS = mkLocale "en" Country.unitedStatesOfAmerica

-- | English (United Kingdom)
enGB :: TmdbLocale
enGB = mkLocale "en" Country.unitedKingdomOfGreatBritainAndNorthernIreland

-- | Chinese (China)
zhCN :: TmdbLocale
zhCN = mkLocale "zh" Country.china

-- | Chinese (Taiwan)
zhTW :: TmdbLocale
zhTW = mkLocale "zh" Country.taiwanProvinceOfChina

-- | Japanese (Japan)
jaJP :: TmdbLocale
jaJP = mkLocale "ja" Country.japan

-- | Korean (South Korea)
koKR :: TmdbLocale
koKR = mkLocale "ko" Country.koreaRepublicOf

-- | French (France)
frFR :: TmdbLocale
frFR = mkLocale "fr" Country.france

-- | German (Germany)
deDE :: TmdbLocale
deDE = mkLocale "de" Country.germany

-- | Spanish (Spain)
esES :: TmdbLocale
esES = mkLocale "es" Country.spain

-- | Spanish (Mexico)
esMX :: TmdbLocale
esMX = mkLocale "es" Country.mexico

-- | Portuguese (Brazil)
ptBR :: TmdbLocale
ptBR = mkLocale "pt" Country.brazil

-- | Portuguese (Portugal)
ptPT :: TmdbLocale
ptPT = mkLocale "pt" Country.portugal

-- | Italian (Italy)
itIT :: TmdbLocale
itIT = mkLocale "it" Country.italy

-- | Russian (Russia)
ruRU :: TmdbLocale
ruRU = mkLocale "ru" Country.russianFederation
