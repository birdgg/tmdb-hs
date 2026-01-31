{-# LANGUAGE FieldSelectors #-}

{- | Discover API routes

Provides endpoints for discovering TV shows and movies.
-}
module Network.Tmdb.API.Discover
  ( DiscoverRoutes (..)
  )
where

import Data.Text (Text)
import GHC.Generics (Generic)
import Network.Tmdb.Types
import Servant.API

-- | Discover API routes
data DiscoverRoutes mode = DiscoverRoutes
  { discoverTv
      :: mode
        :- "tv"
          :> QueryParam' '[Required, Strict] "api_key" Text
          :> QueryParam' '[Required, Strict] "language" TmdbLocale
          :> QueryParam "with_genres" Text
          :> QueryParam "with_text_query" Text
          :> Get '[JSON] (PaginatedResponse TvShow)
  }
  deriving stock (Generic)
