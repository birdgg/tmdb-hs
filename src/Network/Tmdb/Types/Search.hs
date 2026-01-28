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
import Data.Text qualified as Text
import GHC.Generics (Generic)

-- | Media type for multi search results
data MediaType = MediaMovie | MediaTv | MediaPerson
  deriving stock (Show, Eq, Generic)

instance FromJSON MediaType where
  parseJSON = withText "MediaType" $ \case
    "movie" -> pure MediaMovie
    "tv" -> pure MediaTv
    "person" -> pure MediaPerson
    other -> fail $ "Unknown media_type: " <> Text.unpack other

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
  , firstAirDate :: Maybe Text
  -- ^ For TV shows
  , releaseDate :: Maybe Text
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
      <*> o .:? "first_air_date"
      <*> o .:? "release_date"
