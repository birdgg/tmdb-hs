{-# LANGUAGE FieldSelectors #-}

{- | Movie API routes

Provides endpoints for movie details.
-}
module Network.Tmdb.API.Movie
  ( MovieRoutes (..)
  )
where

import Data.Text (Text)
import GHC.Generics (Generic)
import Network.Tmdb.Types
import Servant.API

-- | Movie API routes
data MovieRoutes mode = MovieRoutes
  { getMovieDetail
      :: mode
        :- Capture "movie_id" MovieId
          :> QueryParam' '[Required, Strict] "api_key" Text
          :> QueryParam' '[Required, Strict] "language" TmdbLocale
          :> Get '[JSON] MovieDetail
  }
  deriving stock (Generic)
