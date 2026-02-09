-- | TV Show types for TMDB API
module Network.Tmdb.Types.Tv
  ( -- * TV Show
    TvShow (..)
  , TvDetail (..)
  , TvStatus (..)
  , TvSeasonSummary (..)

    -- * TV Season
  , TvSeasonDetail (..)

    -- * TV Episode
  , TvEpisode (..)
  , TvEpisodeDetail (..)
  )
where

import Data.Aeson
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time.Calendar (Day)
import GHC.Generics (Generic)
import Network.Tmdb.Types.Common (EpisodeId, GenreId, SeasonId, TvShowId, parseOptionalDate)

-- | TV show production/airing status from TMDB API
data TvStatus
  = TvReturning
  | TvPlanned
  | TvInProduction
  | TvEnded
  | TvCanceled
  | TvPilot
  | TvStatusUnknown Text
  deriving stock (Show, Eq, Generic)

instance FromJSON TvStatus where
  parseJSON = withText "TvStatus" $ \case
    "Returning Series" -> pure TvReturning
    "Planned" -> pure TvPlanned
    "In Production" -> pure TvInProduction
    "Ended" -> pure TvEnded
    "Canceled" -> pure TvCanceled
    "Pilot" -> pure TvPilot
    other -> pure (TvStatusUnknown other)

-- | Season summary from TMDB API (included in TvDetail.seasons)
data TvSeasonSummary = TvSeasonSummary
  { seasonNumber :: Int
  , name :: Text
  , episodeCount :: Int
  , airDate :: Maybe Day
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON TvSeasonSummary where
  parseJSON = withObject "TvSeasonSummary" $ \o ->
    TvSeasonSummary
      <$> o .: "season_number"
      <*> o .: "name"
      <*> o .: "episode_count"
      <*> parseOptionalDate o "air_date"

-- | TV Show from TMDB API (discover/search results)
data TvShow = TvShow
  { id :: TvShowId
  , name :: Text
  , originalName :: Text
  , overview :: Text
  , posterPath :: Maybe Text
  , backdropPath :: Maybe Text
  , firstAirDate :: Maybe Day
  , voteAverage :: Double
  , voteCount :: Int64
  , popularity :: Double
  , genreIds :: [GenreId]
  , originCountry :: [Text]
  , originalLanguage :: Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON TvShow where
  parseJSON = withObject "TvShow" $ \o ->
    TvShow
      <$> o .: "id"
      <*> o .: "name"
      <*> o .: "original_name"
      <*> o .: "overview"
      <*> o .:? "poster_path"
      <*> o .:? "backdrop_path"
      <*> parseOptionalDate o "first_air_date"
      <*> o .: "vote_average"
      <*> o .: "vote_count"
      <*> o .: "popularity"
      <*> o .:? "genre_ids" .!= []
      <*> o .:? "origin_country" .!= []
      <*> o .: "original_language"

-- | TV Detail from TMDB API (tv/{id} endpoint)
data TvDetail = TvDetail
  { id :: TvShowId
  , name :: Text
  , originalName :: Text
  , overview :: Text
  , posterPath :: Maybe Text
  , backdropPath :: Maybe Text
  , firstAirDate :: Maybe Day
  , lastAirDate :: Maybe Day
  , voteAverage :: Double
  , voteCount :: Int64
  , popularity :: Double
  , originCountry :: [Text]
  , originalLanguage :: Text
  , status :: TvStatus
  , numberOfSeasons :: Int64
  , numberOfEpisodes :: Int64
  , homepage :: Maybe Text
  , seasons :: [TvSeasonSummary]
  -- ^ List of seasons with episode counts (used for absolute episode conversion)
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON TvDetail where
  parseJSON = withObject "TvDetail" $ \o ->
    TvDetail
      <$> o .: "id"
      <*> o .: "name"
      <*> o .: "original_name"
      <*> o .: "overview"
      <*> o .:? "poster_path"
      <*> o .:? "backdrop_path"
      <*> parseOptionalDate o "first_air_date"
      <*> parseOptionalDate o "last_air_date"
      <*> o .: "vote_average"
      <*> o .: "vote_count"
      <*> o .: "popularity"
      <*> o .:? "origin_country" .!= []
      <*> o .: "original_language"
      <*> o .: "status"
      <*> o .: "number_of_seasons"
      <*> o .: "number_of_episodes"
      <*> o .:? "homepage"
      <*> o .:? "seasons" .!= []

-- | TV Episode from TMDB API (included in TvSeasonDetail.episodes)
data TvEpisode = TvEpisode
  { id :: EpisodeId
  , name :: Text
  , overview :: Text
  , airDate :: Maybe Day
  , episodeNumber :: Int
  , seasonNumber :: Int
  , stillPath :: Maybe Text
  , voteAverage :: Double
  , voteCount :: Int64
  , runtime :: Maybe Int
  , productionCode :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON TvEpisode where
  parseJSON = withObject "TvEpisode" $ \o ->
    TvEpisode
      <$> o .: "id"
      <*> o .: "name"
      <*> o .:? "overview" .!= ""
      <*> parseOptionalDate o "air_date"
      <*> o .: "episode_number"
      <*> o .: "season_number"
      <*> o .:? "still_path"
      <*> o .:? "vote_average" .!= 0
      <*> o .:? "vote_count" .!= 0
      <*> o .:? "runtime"
      <*> o .:? "production_code"

-- | TV Season Detail from TMDB API (tv/{id}/season/{number} endpoint)
data TvSeasonDetail = TvSeasonDetail
  { id :: SeasonId
  , name :: Text
  , overview :: Text
  , posterPath :: Maybe Text
  , seasonNumber :: Int
  , airDate :: Maybe Day
  , voteAverage :: Double
  , voteCount :: Int64
  , episodes :: [TvEpisode]
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON TvSeasonDetail where
  parseJSON = withObject "TvSeasonDetail" $ \o ->
    TvSeasonDetail
      <$> o .: "id"
      <*> o .:? "name" .!= ""
      <*> o .:? "overview" .!= ""
      <*> o .:? "poster_path"
      <*> o .: "season_number"
      <*> parseOptionalDate o "air_date"
      <*> o .:? "vote_average" .!= 0
      <*> o .:? "vote_count" .!= 0
      <*> o .:? "episodes" .!= []

-- | TV Episode Detail from TMDB API (tv/{id}/season/{number}/episode/{number} endpoint)
data TvEpisodeDetail = TvEpisodeDetail
  { id :: EpisodeId
  , showId :: TvShowId
  , name :: Text
  , overview :: Text
  , airDate :: Maybe Day
  , episodeNumber :: Int
  , seasonNumber :: Int
  , stillPath :: Maybe Text
  , voteAverage :: Double
  , voteCount :: Int64
  , runtime :: Maybe Int
  , productionCode :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON TvEpisodeDetail where
  parseJSON = withObject "TvEpisodeDetail" $ \o ->
    TvEpisodeDetail
      <$> o .: "id"
      <*> o .: "show_id"
      <*> o .: "name"
      <*> o .:? "overview" .!= ""
      <*> parseOptionalDate o "air_date"
      <*> o .: "episode_number"
      <*> o .: "season_number"
      <*> o .:? "still_path"
      <*> o .:? "vote_average" .!= 0
      <*> o .:? "vote_count" .!= 0
      <*> o .:? "runtime"
      <*> o .:? "production_code"
