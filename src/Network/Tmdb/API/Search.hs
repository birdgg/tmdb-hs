{-# LANGUAGE FieldSelectors #-}

{- | Search API routes

Provides endpoints for searching TV shows, movies, and people.
-}
module Network.Tmdb.API.Search
  ( SearchRoutes (..)
  )
where

import Data.Text (Text)
import GHC.Generics (Generic)
import Network.Tmdb.Types
import Servant.API

-- | Search API routes
data SearchRoutes mode = SearchRoutes
  { searchTv
      :: mode
        :- "tv"
          :> QueryParam' '[Required, Strict] "api_key" Text
          :> QueryParam' '[Required, Strict] "language" TmdbLocale
          :> QueryParam' '[Required, Strict] "query" Text
          :> Get '[JSON] (PaginatedResponse TvShow)
  , searchMulti
      :: mode
        :- "multi"
          :> QueryParam' '[Required, Strict] "api_key" Text
          :> QueryParam' '[Required, Strict] "language" TmdbLocale
          :> QueryParam' '[Required, Strict] "query" Text
          :> Get '[JSON] (PaginatedResponse MultiSearchResult)
  }
  deriving stock (Generic)
