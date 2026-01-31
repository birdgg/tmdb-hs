-- | Common types for TMDB API
module Network.Tmdb.Types.Common
  ( -- * Pagination
    PaginatedResponse (..)

    -- * Errors
  , TmdbError (..)
  )
where

import Data.Aeson
import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Paginated response from TMDB API
data PaginatedResponse a = PaginatedResponse
  { page :: Int64
  , results :: [a]
  , totalPages :: Int64
  , totalResults :: Int64
  }
  deriving stock (Show, Eq, Generic)

instance (FromJSON a) => FromJSON (PaginatedResponse a) where
  parseJSON = withObject "PaginatedResponse" $ \o ->
    PaginatedResponse
      <$> o .: "page"
      <*> o .: "results"
      <*> o .: "total_pages"
      <*> o .: "total_results"

-- | Errors that can occur during TMDB API operations
data TmdbError
  = -- | Network request failed
    NetworkError Text
  | -- | Failed to parse JSON response
    ParseError Text
  | -- | API returned an error status (status code, message)
    ApiError Int Text
  deriving stock (Show, Eq)
