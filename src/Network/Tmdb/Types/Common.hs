-- | Common types for TMDB API
module Network.Tmdb.Types.Common
  ( -- * ID Types
    MovieId (..),
    TvShowId (..),
    GenreId (..),
    EpisodeId (..),
    SeasonId (..),

    -- * Pagination
    PaginatedResponse (..),

    -- * Date Parsing
    parseOptionalDate,
  )
where

import Data.Aeson (Key, ToJSON, Value (..))
import Data.Aeson.Types (FromJSON (..), Object, Parser, parseMaybe, withObject, (.:), (.:?))
import Data.Int (Int64)
import Data.Text qualified as Text
import Data.Time.Calendar (Day)
import GHC.Generics (Generic)
import Servant.API (FromHttpApiData, ToHttpApiData)

newtype MovieId = MovieId {unMovieId :: Int64}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)

newtype TvShowId = TvShowId {unTvShowId :: Int64}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)

newtype GenreId = GenreId {unGenreId :: Int64}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)

newtype EpisodeId = EpisodeId {unEpisodeId :: Int64}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)

newtype SeasonId = SeasonId {unSeasonId :: Int64}
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

-- | Parse an optional date field from JSON.
-- Handles missing fields, null values, empty/whitespace strings, and
-- invalid date formats as 'Nothing'.
-- Valid dates in YYYY-MM-DD format are parsed as 'Just Day'.
parseOptionalDate :: Object -> Key -> Parser (Maybe Day)
parseOptionalDate o key = do
  mv <- o .:? key
  case mv of
    Nothing -> pure Nothing
    Just Null -> pure Nothing
    Just (String t) | Text.null (Text.strip t) -> pure Nothing
    Just v -> pure (parseMaybe parseJSON v)
