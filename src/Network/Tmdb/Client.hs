{- | TMDB API client using servant-client

This module provides ClientM functions for interacting with the TMDB API.
-}
module Network.Tmdb.Client
  ( -- * API Functions
    discoverTv
  , searchTv
  , searchMulti
  , getMovieDetail
  , getTvDetail
  , getTvSeasonDetail
  , getTvEpisodeDetail

    -- * Re-exports
  , module Network.Tmdb.Types
  , tmdbBaseUrl
  )
where

import Data.Int (Int64)
import Data.Text (Text)
import Network.Tmdb.API (TmdbRoutes (..), tmdbBaseUrl)
import qualified Network.Tmdb.API.Discover as Discover
import qualified Network.Tmdb.API.Movie as Movie
import qualified Network.Tmdb.API.Search as Search
import qualified Network.Tmdb.API.Tv as Tv
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
  Discover.discoverTv (discover client') apiKey lang params.withGenres params.withTextQuery

-- | Search TV shows
searchTv
  :: Text
  -- ^ API key
  -> Text
  -- ^ Language
  -> Text
  -- ^ Search query
  -> ClientM (PaginatedResponse TvShow)
searchTv apiKey lang = Search.searchTv (search client') apiKey lang

-- | Search multi (movies, TV shows, and people)
searchMulti
  :: Text
  -- ^ API key
  -> Text
  -- ^ Language
  -> Text
  -- ^ Search query
  -> ClientM (PaginatedResponse MultiSearchResult)
searchMulti apiKey lang = Search.searchMulti (search client') apiKey lang

-- | Get movie detail
getMovieDetail
  :: Text
  -- ^ API key
  -> Text
  -- ^ Language
  -> Int64
  -- ^ Movie ID
  -> ClientM MovieDetail
getMovieDetail apiKey lang movieId =
  Movie.getMovieDetail (movie client') movieId apiKey lang

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
  Tv.getTvDetail (tv client') tvId apiKey lang

-- | Get TV season detail
getTvSeasonDetail
  :: Text
  -- ^ API key
  -> Text
  -- ^ Language
  -> Int64
  -- ^ TV series ID
  -> Int
  -- ^ Season number
  -> ClientM TvSeasonDetail
getTvSeasonDetail apiKey lang seriesId seasonNum =
  Tv.getTvSeasonDetail (tv client') seriesId seasonNum apiKey lang

-- | Get TV episode detail
getTvEpisodeDetail
  :: Text
  -- ^ API key
  -> Text
  -- ^ Language
  -> Int64
  -- ^ TV series ID
  -> Int
  -- ^ Season number
  -> Int
  -- ^ Episode number
  -> ClientM TvEpisodeDetail
getTvEpisodeDetail apiKey lang seriesId seasonNum episodeNum =
  Tv.getTvEpisodeDetail (tv client') seriesId seasonNum episodeNum apiKey lang
