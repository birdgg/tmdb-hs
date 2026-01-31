-- | Common types for TMDB API
module Network.Tmdb.Types.Common
  ( -- * ID Types
    MovieId (..),
    TvShowId (..),

    -- * Pagination
    PaginatedResponse (..),
  )
where

import Data.Aeson (ToJSON)
import Data.Aeson.Types (FromJSON (..), withObject, (.:))
import Data.Int (Int64)
import GHC.Generics (Generic)
import Servant.API (FromHttpApiData, ToHttpApiData)

newtype MovieId = MovieId {unMovieId :: Int64}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)

newtype TvShowId = TvShowId {unTvShowId :: Int64}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)

data PaginatedResponse a = PaginatedResponse
  { page :: Int64,
    results :: [a],
    totalPages :: Int64,
    totalResults :: Int64
  }
  deriving stock (Show, Eq, Generic)

instance (FromJSON a) => FromJSON (PaginatedResponse a) where
  parseJSON = withObject "PaginatedResponse" $ \o ->
    PaginatedResponse
      <$> o .: "page"
      <*> o .: "results"
      <*> o .: "total_pages"
      <*> o .: "total_results"
