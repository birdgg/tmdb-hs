{- | TMDB API client library

This module re-exports all public API from the tmdb package.

= Usage (IO interface - recommended)

@
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Tmdb

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  let client = newTmdbClient (TmdbConfig "your-api-key" zhCN) manager
  result <- searchTv client "進撃の巨人"
  case result of
    Right shows -> print shows
    Left err -> print err
@

= Usage (Low-level ClientM interface)

For more control, use the low-level 'Network.Tmdb.Client' module directly:

@
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Network.Tmdb.Client as Tmdb
import Servant.Client (mkClientEnv, runClientM)

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  let env = mkClientEnv manager Tmdb.tmdbBaseUrl
  result <- runClientM (Tmdb.searchTv "your-api-key" enUS "Breaking Bad") env
  print result
@
-}
module Network.Tmdb
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

    -- * Types
  , module Network.Tmdb.Types

    -- * Errors
  , ClientError
  )
where

import Network.Tmdb.IO
import Network.Tmdb.Types
