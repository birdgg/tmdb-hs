-- | Types for TMDB API
module Network.Tmdb.Types
  ( -- * TV Show
    TvShow (..)
  , TvDetail (..)
  , TvSeasonSummary (..)

    -- * Multi Search
  , MultiSearchResult (..)
  , MediaType (..)

    -- * Pagination
  , PaginatedResponse (..)

    -- * Request Parameters
  , DiscoverTvParams (..)

    -- * Errors
  , TmdbError (..)

    -- * Configuration
  , defaultLanguage
  )
where

import Network.Tmdb.Types.Common
import Network.Tmdb.Types.Discover
import Network.Tmdb.Types.Search
import Network.Tmdb.Types.Tv
