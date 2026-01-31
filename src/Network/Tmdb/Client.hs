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
  -> TmdbLocale
  -- ^ Language/region locale
  -> DiscoverTvParams
  -- ^ Discovery parameters
  -> ClientM (PaginatedResponse TvShow)
discoverTv apiKey locale params =
  Discover.discoverTv (discover client') apiKey locale params.withGenres params.withTextQuery

-- | Search TV shows
searchTv
  :: Text
  -- ^ API key
  -> TmdbLocale
  -- ^ Language/region locale
  -> Text
  -- ^ Search query
  -> ClientM (PaginatedResponse TvShow)
searchTv apiKey locale = Search.searchTv (search client') apiKey locale

-- | Search multi (movies, TV shows, and people)
searchMulti
  :: Text
  -- ^ API key
  -> TmdbLocale
  -- ^ Language/region locale
  -> Text
  -- ^ Search query
  -> ClientM (PaginatedResponse MultiSearchResult)
searchMulti apiKey locale = Search.searchMulti (search client') apiKey locale

-- | Get movie detail
getMovieDetail
  :: Text
  -- ^ API key
  -> TmdbLocale
  -- ^ Language/region locale
  -> Int64
  -- ^ Movie ID
  -> ClientM MovieDetail
getMovieDetail apiKey locale movieId =
  Movie.getMovieDetail (movie client') movieId apiKey locale

-- | Get TV detail
getTvDetail
  :: Text
  -- ^ API key
  -> TmdbLocale
  -- ^ Language/region locale
  -> Int64
  -- ^ TV ID
  -> ClientM TvDetail
getTvDetail apiKey locale tvId =
  Tv.getTvDetail (tv client') tvId apiKey locale

-- | Get TV season detail
getTvSeasonDetail
  :: Text
  -- ^ API key
  -> TmdbLocale
  -- ^ Language/region locale
  -> Int64
  -- ^ TV series ID
  -> Int
  -- ^ Season number
  -> ClientM TvSeasonDetail
getTvSeasonDetail apiKey locale seriesId seasonNum =
  Tv.getTvSeasonDetail (tv client') seriesId seasonNum apiKey locale

-- | Get TV episode detail
getTvEpisodeDetail
  :: Text
  -- ^ API key
  -> TmdbLocale
  -- ^ Language/region locale
  -> Int64
  -- ^ TV series ID
  -> Int
  -- ^ Season number
  -> Int
  -- ^ Episode number
  -> ClientM TvEpisodeDetail
getTvEpisodeDetail apiKey locale seriesId seasonNum episodeNum =
  Tv.getTvEpisodeDetail (tv client') seriesId seasonNum episodeNum apiKey locale
