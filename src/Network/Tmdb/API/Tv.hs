{-# LANGUAGE FieldSelectors #-}

{- | TV API routes

Provides endpoints for TV show details, seasons, and episodes.
-}
module Network.Tmdb.API.Tv
  ( TvRoutes (..)
  )
where

import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.Tmdb.Types
import Servant.API

-- | TV API routes
data TvRoutes mode = TvRoutes
  { getTvDetail
      :: mode
        :- Capture "tv_id" Int64
          :> QueryParam' '[Required, Strict] "api_key" Text
          :> QueryParam' '[Required, Strict] "language" TmdbLocale
          :> Get '[JSON] TvDetail
  , getTvSeasonDetail
      :: mode
        :- Capture "series_id" Int64
          :> "season"
          :> Capture "season_number" Int
          :> QueryParam' '[Required, Strict] "api_key" Text
          :> QueryParam' '[Required, Strict] "language" TmdbLocale
          :> Get '[JSON] TvSeasonDetail
  , getTvEpisodeDetail
      :: mode
        :- Capture "series_id" Int64
          :> "season"
          :> Capture "season_number" Int
          :> "episode"
          :> Capture "episode_number" Int
          :> QueryParam' '[Required, Strict] "api_key" Text
          :> QueryParam' '[Required, Strict] "language" TmdbLocale
          :> Get '[JSON] TvEpisodeDetail
  }
  deriving stock (Generic)
