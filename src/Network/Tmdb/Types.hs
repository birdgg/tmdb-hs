-- | Types for TMDB API
module Network.Tmdb.Types
  ( -- * ID Types
    MovieId (..)
  , TvShowId (..)
  , GenreId (..)
  , EpisodeId (..)
  , SeasonId (..)

    -- * Movie
  , Movie (..)
  , MovieDetail (..)
  , MovieStatus (..)
  , Genre (..)
  , ProductionCompany (..)
  , ProductionCountry (..)
  , SpokenLanguage (..)

    -- * TV Show
  , TvShow (..)
  , TvDetail (..)
  , TvStatus (..)
  , TvSeasonSummary (..)

    -- * TV Season
  , TvSeasonDetail (..)

    -- * TV Episode
  , TvEpisode (..)
  , TvEpisodeDetail (..)

    -- * Multi Search
  , MultiSearchResult (..)
  , MediaType (..)

    -- * Pagination
  , PaginatedResponse (..)

    -- * Request Parameters
  , DiscoverTvParams (..)

    -- * Errors
  , TmdbError (..)
  , ApiErrorResponse (..)
  , fromClientError
  , isNotFound
  , isAuthError
  , isRateLimited
  , isNetworkError

    -- * Language and Locale
  , TmdbLocale (..)
  , mkLocale
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

    -- * Date
  , Day

    -- * Image Configuration
  , imageBaseUrl
  , PosterSize (..)
  , BackdropSize (..)
  , ProfileSize (..)
  , LogoSize (..)
  , StillSize (..)
  , posterUrl
  , backdropUrl
  , profileUrl
  , logoUrl
  , stillUrl
  )
where

import Data.Time.Calendar (Day)
import Network.Tmdb.Types.Common
import Network.Tmdb.Types.Discover
import Network.Tmdb.Types.Error
import Network.Tmdb.Types.Image
import Network.Tmdb.Types.Language
import Network.Tmdb.Types.Movie
import Network.Tmdb.Types.Search
import Network.Tmdb.Types.Tv
