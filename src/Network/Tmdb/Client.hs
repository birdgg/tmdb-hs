{- | TMDB API client using servant-client

This module provides ClientM functions for interacting with the TMDB API.
-}
module Network.Tmdb.Client
  ( -- * API Functions
    discoverTv
  , searchTv
  , searchMulti
  , getTvDetail

    -- * Re-exports
  , module Network.Tmdb.Types
  , tmdbBaseUrl
  )
where

import Data.Int (Int64)
import Data.Text (Text)
import Network.Tmdb.API (TmdbRoutes (..), tmdbBaseUrl)
import Network.Tmdb.Types
import Servant.Client (ClientM)
import Servant.Client.Generic (AsClientT, genericClient)

-- | Generated client functions record (internal)
client' :: TmdbRoutes (AsClientT ClientM)
client' = genericClient

-- | Discover TV shows with optional filters
discoverTv
  :: Text
  -- ^ API key
  -> Text
  -- ^ Language (e.g. "zh-CN", "en-US")
  -> DiscoverTvParams
  -- ^ Discovery parameters
  -> ClientM (PaginatedResponse TvShow)
discoverTv apiKey lang params =
  client'.discoverTv apiKey lang params.withGenres params.withTextQuery

-- | Search TV shows
searchTv
  :: Text
  -- ^ API key
  -> Text
  -- ^ Language
  -> Text
  -- ^ Search query
  -> ClientM (PaginatedResponse TvShow)
searchTv apiKey lang = client'.searchTv apiKey lang

-- | Search multi (movies, TV shows, and people)
searchMulti
  :: Text
  -- ^ API key
  -> Text
  -- ^ Language
  -> Text
  -- ^ Search query
  -> ClientM (PaginatedResponse MultiSearchResult)
searchMulti apiKey lang = client'.searchMulti apiKey lang

-- | Get TV detail
getTvDetail
  :: Text
  -- ^ API key
  -> Text
  -- ^ Language
  -> Int64
  -- ^ TV ID
  -> ClientM TvDetail
getTvDetail apiKey lang tvId =
  client'.getTvDetail tvId apiKey lang
