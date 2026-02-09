{-# LANGUAGE FieldSelectors #-}

-- | TMDB API definition for servant-client
module Network.Tmdb.API
  ( -- * API Types
    TmdbAPI
  , TmdbAuth
  , TmdbRoutes (..)

    -- * Sub-routes
  , DiscoverRoutes (..)
  , MovieRoutes (..)
  , SearchRoutes (..)
  , TvRoutes (..)

    -- * Configuration
  , tmdbBaseUrl
  )
where

import Data.Text (Text)
import GHC.Generics (Generic)
import Network.Tmdb.Types
import Servant.API
import Servant.Client (BaseUrl (..), Scheme (..))

-- | Common query parameters required by all TMDB API endpoints
type TmdbAuth api =
  QueryParam' '[Required, Strict] "api_key" Text
    :> QueryParam' '[Required, Strict] "language" TmdbLocale
    :> api

-- | TMDB API v3
type TmdbAPI = TmdbAuth (NamedRoutes TmdbRoutes)

-- | TMDB API routes using NamedRoutes pattern
data TmdbRoutes mode = TmdbRoutes
  { discover :: mode :- "discover" :> NamedRoutes DiscoverRoutes
  , movie :: mode :- "movie" :> NamedRoutes MovieRoutes
  , search :: mode :- "search" :> NamedRoutes SearchRoutes
  , tv :: mode :- "tv" :> NamedRoutes TvRoutes
  }
  deriving stock (Generic)

-- | Discover API routes
data DiscoverRoutes mode = DiscoverRoutes
  { discoverTv
      :: mode
        :- "tv"
          :> QueryParam "with_genres" Text
          :> QueryParam "with_text_query" Text
          :> Get '[JSON] (PaginatedResponse TvShow)
  }
  deriving stock (Generic)

-- | Movie API routes
data MovieRoutes mode = MovieRoutes
  { getMovieDetail
      :: mode
        :- Capture "movie_id" MovieId
          :> Get '[JSON] MovieDetail
  }
  deriving stock (Generic)

-- | Search API routes
data SearchRoutes mode = SearchRoutes
  { searchTv
      :: mode
        :- "tv"
          :> QueryParam' '[Required, Strict] "query" Text
          :> Get '[JSON] (PaginatedResponse TvShow)
  , searchMulti
      :: mode
        :- "multi"
          :> QueryParam' '[Required, Strict] "query" Text
          :> Get '[JSON] (PaginatedResponse MultiSearchResult)
  }
  deriving stock (Generic)

-- | TV API routes
data TvRoutes mode = TvRoutes
  { getTvDetail
      :: mode
        :- Capture "tv_id" TvShowId
          :> Get '[JSON] TvDetail
  , getTvSeasonDetail
      :: mode
        :- Capture "series_id" TvShowId
          :> "season"
          :> Capture "season_number" Int
          :> Get '[JSON] TvSeasonDetail
  , getTvEpisodeDetail
      :: mode
        :- Capture "series_id" TvShowId
          :> "season"
          :> Capture "season_number" Int
          :> "episode"
          :> Capture "episode_number" Int
          :> Get '[JSON] TvEpisodeDetail
  }
  deriving stock (Generic)

-- | Base URL for TMDB API v3
tmdbBaseUrl :: BaseUrl
tmdbBaseUrl = BaseUrl Https "api.themoviedb.org" 443 "/3"
