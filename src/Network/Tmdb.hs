{- | TMDB API client library

= Usage

@
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Tmdb

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  let tmdb = mkTmdbClient (TmdbConfig "your-api-key" zhCN) manager
  result <- tmdb.searchTv "\36914\25731\12398\24040\20154"
  case result of
    Right shows -> print shows
    Left err -> print err
@
-}
module Network.Tmdb
  ( -- * API
    TmdbApi (..)
  , TmdbConfig (..)
  , mkTmdbClient

    -- * Types
  , module Network.Tmdb.Types

    -- * Errors
  , TmdbClientError (..)
  , TmdbApiError (..)
  )
where

import Data.Bifunctor (first)
import Data.Text (Text)
import Network.HTTP.Client (Manager)
import Network.Tmdb.API (tmdbBaseUrl)
import qualified Network.Tmdb.API as API
import Network.Tmdb.Types
import Data.Proxy (Proxy (..))
import Servant.Client (ClientEnv, ClientM, client, mkClientEnv, runClientM)
import Servant.Client.Generic (AsClientT)

-- | Configuration for TMDB API
data TmdbConfig = TmdbConfig
  { apiKey :: Text
  -- ^ Your TMDB API key
  , language :: TmdbLocale
  -- ^ Locale for results (e.g. zhCN, enUS, jaJP)
  }
  deriving stock (Show, Eq)

-- | TMDB API
--
-- Use record dot syntax to call API methods:
--
-- @
-- let tmdb = mkTmdbClient config manager
-- result <- tmdb.searchTv "query"
-- detail <- tmdb.getTvDetail tvId
-- @
data TmdbApi = TmdbApi
  { searchTv :: Text -> IO (Either TmdbClientError (PaginatedResponse TvShow))
  -- ^ Search TV shows
  , searchMulti :: Text -> IO (Either TmdbClientError (PaginatedResponse MultiSearchResult))
  -- ^ Search multi (movies, TV shows, and people)
  , discoverTv :: DiscoverTvParams -> IO (Either TmdbClientError (PaginatedResponse TvShow))
  -- ^ Discover TV shows with optional filters
  , getMovieDetail :: MovieId -> IO (Either TmdbClientError MovieDetail)
  -- ^ Get movie details
  , getTvDetail :: TvShowId -> IO (Either TmdbClientError TvDetail)
  -- ^ Get TV show details
  , getTvSeasonDetail :: TvShowId -> Int -> IO (Either TmdbClientError TvSeasonDetail)
  -- ^ Get TV season details
  , getTvEpisodeDetail :: TvShowId -> Int -> Int -> IO (Either TmdbClientError TvEpisodeDetail)
  -- ^ Get TV episode details
  }

-- | Create a new TMDB API client
--
-- You need to create a 'Manager' yourself and pass it in.
-- This gives you full control over the HTTP manager's lifecycle.
--
-- @
-- import Network.HTTP.Client (newManager)
-- import Network.HTTP.Client.TLS (tlsManagerSettings)
-- import Network.Tmdb
--
-- main :: IO ()
-- main = do
--   manager <- newManager tlsManagerSettings
--   let tmdb = mkTmdbClient (TmdbConfig "your-api-key" zhCN) manager
--   result <- tmdb.searchTv "\36914\25731\12398\24040\20154"
--   print result
-- @
mkTmdbClient :: TmdbConfig -> Manager -> TmdbApi
mkTmdbClient cfg manager =
  TmdbApi
    { searchTv = \query ->
        run (API.searchTv (API.search routes) query)
    , searchMulti = \query ->
        run (API.searchMulti (API.search routes) query)
    , discoverTv = \params ->
        run (API.discoverTv (API.discover routes) params.withGenres params.withTextQuery)
    , getMovieDetail = \movieId ->
        run (API.getMovieDetail (API.movie routes) movieId)
    , getTvDetail = \tvId ->
        run (API.getTvDetail (API.tv routes) tvId)
    , getTvSeasonDetail = \seriesId seasonNum ->
        run (API.getTvSeasonDetail (API.tv routes) seriesId seasonNum)
    , getTvEpisodeDetail = \seriesId seasonNum episodeNum ->
        run (API.getTvEpisodeDetail (API.tv routes) seriesId seasonNum episodeNum)
    }
  where
    env :: ClientEnv
    env = mkClientEnv manager tmdbBaseUrl

    routes :: API.TmdbRoutes (AsClientT ClientM)
    routes = client (Proxy @API.TmdbAPI) cfg.apiKey cfg.language

    run :: ClientM a -> IO (Either TmdbClientError a)
    run action = first fromClientError <$> runClientM action env
