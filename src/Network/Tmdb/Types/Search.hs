-- | Search types for TMDB API
module Network.Tmdb.Types.Search
  ( -- * Multi Search
    MultiSearchResult (..)
  , MediaType (..)
  )
where

import Data.Aeson
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time.Calendar (Day)
import GHC.Generics (Generic)
import Network.Tmdb.Types.Common (parseOptionalDate)

-- | Media type for multi search results
data MediaType = MediaMovie | MediaTv | MediaPerson | MediaUnknown Text
  deriving stock (Show, Eq, Generic)

instance FromJSON MediaType where
  parseJSON = withText "MediaType" $ \case
    "movie" -> pure MediaMovie
    "tv" -> pure MediaTv
    "person" -> pure MediaPerson
    other -> pure (MediaUnknown other)

-- | Multi search result from TMDB API
-- This is a union type that can represent movie, tv, or person results
data MultiSearchResult = MultiSearchResult
  { id :: Int64
  , mediaType :: MediaType
  , name :: Maybe Text
  -- ^ For TV shows
  , title :: Maybe Text
  -- ^ For movies
  , originalName :: Maybe Text
  -- ^ For TV shows
  , originalTitle :: Maybe Text
  -- ^ For movies
  , posterPath :: Maybe Text
  , firstAirDate :: Maybe Day
  -- ^ For TV shows
  , releaseDate :: Maybe Day
  -- ^ For movies
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON MultiSearchResult where
  parseJSON = withObject "MultiSearchResult" $ \o ->
    MultiSearchResult
      <$> o .: "id"
      <*> o .: "media_type"
      <*> o .:? "name"
      <*> o .:? "title"
      <*> o .:? "original_name"
      <*> o .:? "original_title"
      <*> o .:? "poster_path"
      <*> parseOptionalDate o "first_air_date"
      <*> parseOptionalDate o "release_date"
