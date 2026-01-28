{-# LANGUAGE FieldSelectors #-}

{- | TMDB API definition for servant-client

This module combines all API routes into a unified API type.
-}
module Network.Tmdb.API
  ( -- * API Types
    TmdbAPI
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

import GHC.Generics (Generic)
import Network.Tmdb.API.Discover (DiscoverRoutes (..))
import Network.Tmdb.API.Movie (MovieRoutes (..))
import Network.Tmdb.API.Search (SearchRoutes (..))
import Network.Tmdb.API.Tv (TvRoutes (..))
import Servant.API
import Servant.Client (BaseUrl (..), Scheme (..))

-- | TMDB API v3
type TmdbAPI = "3" :> NamedRoutes TmdbRoutes

-- | TMDB API routes using NamedRoutes pattern
data TmdbRoutes mode = TmdbRoutes
  { discover :: mode :- "discover" :> NamedRoutes DiscoverRoutes
  , movie :: mode :- "movie" :> NamedRoutes MovieRoutes
  , search :: mode :- "search" :> NamedRoutes SearchRoutes
  , tv :: mode :- "tv" :> NamedRoutes TvRoutes
  }
  deriving stock (Generic)

-- | Base URL for TMDB API
tmdbBaseUrl :: BaseUrl
tmdbBaseUrl = BaseUrl Https "api.themoviedb.org" 443 "/3"
