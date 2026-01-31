{- | High-level IO interface for TMDB API

This module provides a simple IO-based interface for interacting with
the TMDB API.

= Usage

@
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Tmdb.IO

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  let client = newTmdbClient (TmdbConfig "your-api-key" zhCN) manager
  result <- searchTv client "進撃の巨人"
  case result of
    Right shows -> print shows
    Left err -> print err
@
-}
module Network.Tmdb.IO
  ( -- * Client
    TmdbClient
  , TmdbConfig (..)
  , newTmdbClient

    -- * API Functions
  , searchTv
  , searchMulti
  , discoverTv
  , getMovieDetail
  , getTvDetail
  , getTvSeasonDetail
  , getTvEpisodeDetail

    -- * Re-exports
  , ClientError
  )
where

import Data.Int (Int64)
import Data.Text (Text)
import Network.HTTP.Client (Manager)
import Network.Tmdb.API (tmdbBaseUrl)
import qualified Network.Tmdb.Client as C
import Network.Tmdb.Types
import Servant.Client (ClientEnv, ClientError, mkClientEnv, runClientM)

-- | Configuration for TMDB client
data TmdbConfig = TmdbConfig
  { apiKey :: Text
  -- ^ Your TMDB API key
  , locale :: TmdbLocale
  -- ^ Locale for results (e.g. zhCN, enUS, jaJP)
  }
  deriving stock (Show, Eq)

-- | TMDB client handle
data TmdbClient = TmdbClient
  { config :: TmdbConfig
  , clientEnv :: ClientEnv
  }

-- | Create a new TMDB client
--
-- You need to create a 'Manager' yourself and pass it in.
-- This gives you full control over the HTTP manager's lifecycle
-- and allows for connection reuse across multiple clients.
--
-- @
-- import Network.HTTP.Client (newManager)
-- import Network.HTTP.Client.TLS (tlsManagerSettings)
--
-- manager <- newManager tlsManagerSettings
-- let client = newTmdbClient config manager
-- @
newTmdbClient :: TmdbConfig -> Manager -> TmdbClient
newTmdbClient cfg manager =
  TmdbClient
    { config = cfg
    , clientEnv = mkClientEnv manager tmdbBaseUrl
    }

-- | Search TV shows
searchTv
  :: TmdbClient
  -> Text
  -- ^ Search query
  -> IO (Either ClientError (PaginatedResponse TvShow))
searchTv client query =
  runClientM
    (C.searchTv client.config.apiKey client.config.locale query)
    client.clientEnv

-- | Search multi (movies, TV shows, and people)
searchMulti
  :: TmdbClient
  -> Text
  -- ^ Search query
  -> IO (Either ClientError (PaginatedResponse MultiSearchResult))
searchMulti client query =
  runClientM
    (C.searchMulti client.config.apiKey client.config.locale query)
    client.clientEnv

-- | Discover TV shows with optional filters
discoverTv
  :: TmdbClient
  -> DiscoverTvParams
  -- ^ Discovery parameters
  -> IO (Either ClientError (PaginatedResponse TvShow))
discoverTv client params =
  runClientM
    (C.discoverTv client.config.apiKey client.config.locale params)
    client.clientEnv

-- | Get movie details
getMovieDetail
  :: TmdbClient
  -> Int64
  -- ^ Movie ID
  -> IO (Either ClientError MovieDetail)
getMovieDetail client movieId =
  runClientM
    (C.getMovieDetail client.config.apiKey client.config.locale movieId)
    client.clientEnv

-- | Get TV show details
getTvDetail
  :: TmdbClient
  -> Int64
  -- ^ TV show ID
  -> IO (Either ClientError TvDetail)
getTvDetail client tvId =
  runClientM
    (C.getTvDetail client.config.apiKey client.config.locale tvId)
    client.clientEnv

-- | Get TV season details
getTvSeasonDetail
  :: TmdbClient
  -> Int64
  -- ^ TV series ID
  -> Int
  -- ^ Season number
  -> IO (Either ClientError TvSeasonDetail)
getTvSeasonDetail client seriesId seasonNum =
  runClientM
    (C.getTvSeasonDetail client.config.apiKey client.config.locale seriesId seasonNum)
    client.clientEnv

-- | Get TV episode details
getTvEpisodeDetail
  :: TmdbClient
  -> Int64
  -- ^ TV series ID
  -> Int
  -- ^ Season number
  -> Int
  -- ^ Episode number
  -> IO (Either ClientError TvEpisodeDetail)
getTvEpisodeDetail client seriesId seasonNum episodeNum =
  runClientM
    (C.getTvEpisodeDetail client.config.apiKey client.config.locale seriesId seasonNum episodeNum)
    client.clientEnv
