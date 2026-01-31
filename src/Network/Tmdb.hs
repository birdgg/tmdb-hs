{- | TMDB API client library

This module re-exports all public API from the tmdb package.

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
module Network.Tmdb
  ( -- * API
    TmdbApi (..)
  , TmdbConfig (..)
  , newTmdbApi

    -- * Types
  , module Network.Tmdb.Types

    -- * Errors
  , ClientError
  )
where

import Network.Tmdb.Client
import Network.Tmdb.Types
