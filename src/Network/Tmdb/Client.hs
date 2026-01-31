{- | TMDB API client

= Usage

@
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Tmdb

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  let tmdb = newTmdbApi (TmdbConfig "your-api-key" zhCN) manager
  result <- tmdb.searchTv "進撃の巨人"
  case result of
    Right shows -> print shows
    Left err -> print err
@
-}
module Network.Tmdb.Client
  ( -- * API
    TmdbApi (..)
  , TmdbConfig (..)
  , newTmdbApi

    -- * Re-exports
  , module Network.Tmdb.Types
  , tmdbBaseUrl
  , ClientError
  )
where

import Data.Text (Text)
import Network.HTTP.Client (Manager)
import Network.Tmdb.API (TmdbRoutes (..), tmdbBaseUrl)
import qualified Network.Tmdb.API.Discover as Discover
import qualified Network.Tmdb.API.Movie as Movie
import qualified Network.Tmdb.API.Search as Search
import qualified Network.Tmdb.API.Tv as Tv
import Network.Tmdb.Types
import Data.Bifunctor (first)
import Servant.Client (ClientEnv, ClientError, ClientM, mkClientEnv, runClientM)
import Servant.Client.Generic (AsClientT, genericClient)

-- | Configuration for TMDB API
data TmdbConfig = TmdbConfig
  { apiKey :: Text
  -- ^ Your TMDB API key
  , locale :: TmdbLocale
  -- ^ Locale for results (e.g. zhCN, enUS, jaJP)
  }
  deriving stock (Show, Eq)

-- | TMDB API
--
-- Use record dot syntax to call API methods:
--
-- @
-- let tmdb = newTmdbApi config manager
-- result <- tmdb.searchTv "query"
-- detail <- tmdb.getTvDetail tvId
-- @
data TmdbApi = TmdbApi
  { searchTv :: Text -> IO (Either TmdbError (PaginatedResponse TvShow))
  -- ^ Search TV shows
  , searchMulti :: Text -> IO (Either TmdbError (PaginatedResponse MultiSearchResult))
  -- ^ Search multi (movies, TV shows, and people)
  , discoverTv :: DiscoverTvParams -> IO (Either TmdbError (PaginatedResponse TvShow))
  -- ^ Discover TV shows with optional filters
  , getMovieDetail :: MovieId -> IO (Either TmdbError MovieDetail)
  -- ^ Get movie details
  , getTvDetail :: TvShowId -> IO (Either TmdbError TvDetail)
  -- ^ Get TV show details
  , getTvSeasonDetail :: TvShowId -> Int -> IO (Either TmdbError TvSeasonDetail)
  -- ^ Get TV season details
  , getTvEpisodeDetail :: TvShowId -> Int -> Int -> IO (Either TmdbError TvEpisodeDetail)
  -- ^ Get TV episode details
  }

-- | Create a new TMDB API
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
--   let tmdb = newTmdbApi (TmdbConfig "your-api-key" zhCN) manager
--   result <- tmdb.searchTv "進撃の巨人"
--   print result
-- @
newTmdbApi :: TmdbConfig -> Manager -> TmdbApi
newTmdbApi cfg manager =
  TmdbApi
    { searchTv = \query ->
        first fromClientError
          <$> runClientM
            (Search.searchTv (search client') cfg.apiKey cfg.locale query)
            env
    , searchMulti = \query ->
        first fromClientError
          <$> runClientM
            (Search.searchMulti (search client') cfg.apiKey cfg.locale query)
            env
    , discoverTv = \params ->
        first fromClientError
          <$> runClientM
            (Discover.discoverTv (discover client') cfg.apiKey cfg.locale params.withGenres params.withTextQuery)
            env
    , getMovieDetail = \movieId ->
        first fromClientError
          <$> runClientM
            (Movie.getMovieDetail (movie client') movieId cfg.apiKey cfg.locale)
            env
    , getTvDetail = \tvId ->
        first fromClientError
          <$> runClientM
            (Tv.getTvDetail (tv client') tvId cfg.apiKey cfg.locale)
            env
    , getTvSeasonDetail = \seriesId seasonNum ->
        first fromClientError
          <$> runClientM
            (Tv.getTvSeasonDetail (tv client') seriesId seasonNum cfg.apiKey cfg.locale)
            env
    , getTvEpisodeDetail = \seriesId seasonNum episodeNum ->
        first fromClientError
          <$> runClientM
            (Tv.getTvEpisodeDetail (tv client') seriesId seasonNum episodeNum cfg.apiKey cfg.locale)
            env
    }
  where
    env :: ClientEnv
    env = mkClientEnv manager tmdbBaseUrl

    client' :: TmdbRoutes (AsClientT ClientM)
    client' = genericClient
