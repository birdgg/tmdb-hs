-- | Movie types for TMDB API
module Network.Tmdb.Types.Movie
  ( -- * Movie
    Movie (..)
  , MovieDetail (..)

    -- * Supporting types
  , Genre (..)
  , ProductionCompany (..)
  , ProductionCountry (..)
  , SpokenLanguage (..)
  )
where

import Data.Aeson
import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.Tmdb.Types.Common (MovieId)

-- | Genre from TMDB API
data Genre = Genre
  { id :: Int64
  , name :: Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON Genre where
  parseJSON = withObject "Genre" $ \o ->
    Genre
      <$> o .: "id"
      <*> o .: "name"

-- | Production company from TMDB API
data ProductionCompany = ProductionCompany
  { id :: Int64
  , name :: Text
  , logoPath :: Maybe Text
  , originCountry :: Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON ProductionCompany where
  parseJSON = withObject "ProductionCompany" $ \o ->
    ProductionCompany
      <$> o .: "id"
      <*> o .: "name"
      <*> o .:? "logo_path"
      <*> o .:? "origin_country" .!= ""

-- | Production country from TMDB API
data ProductionCountry = ProductionCountry
  { iso31661 :: Text
  , name :: Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON ProductionCountry where
  parseJSON = withObject "ProductionCountry" $ \o ->
    ProductionCountry
      <$> o .: "iso_3166_1"
      <*> o .: "name"

-- | Spoken language from TMDB API
data SpokenLanguage = SpokenLanguage
  { iso6391 :: Text
  , name :: Text
  , englishName :: Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON SpokenLanguage where
  parseJSON = withObject "SpokenLanguage" $ \o ->
    SpokenLanguage
      <$> o .: "iso_639_1"
      <*> o .: "name"
      <*> o .:? "english_name" .!= ""

-- | Movie from TMDB API (discover/search results)
data Movie = Movie
  { id :: MovieId
  , title :: Text
  , originalTitle :: Text
  , overview :: Text
  , posterPath :: Maybe Text
  , backdropPath :: Maybe Text
  , releaseDate :: Maybe Text
  , voteAverage :: Double
  , voteCount :: Int64
  , popularity :: Double
  , genreIds :: [Int64]
  , originalLanguage :: Text
  , adult :: Bool
  , video :: Bool
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON Movie where
  parseJSON = withObject "Movie" $ \o ->
    Movie
      <$> o .: "id"
      <*> o .: "title"
      <*> o .: "original_title"
      <*> o .:? "overview" .!= ""
      <*> o .:? "poster_path"
      <*> o .:? "backdrop_path"
      <*> o .:? "release_date"
      <*> o .: "vote_average"
      <*> o .: "vote_count"
      <*> o .: "popularity"
      <*> o .:? "genre_ids" .!= []
      <*> o .: "original_language"
      <*> o .:? "adult" .!= False
      <*> o .:? "video" .!= False

-- | Movie Detail from TMDB API (movie/{id} endpoint)
data MovieDetail = MovieDetail
  { id :: MovieId
  , title :: Text
  , originalTitle :: Text
  , overview :: Text
  , tagline :: Maybe Text
  , posterPath :: Maybe Text
  , backdropPath :: Maybe Text
  , releaseDate :: Maybe Text
  , runtime :: Maybe Int
  , voteAverage :: Double
  , voteCount :: Int64
  , popularity :: Double
  , genres :: [Genre]
  , originalLanguage :: Text
  , adult :: Bool
  , video :: Bool
  , status :: Text
  , homepage :: Maybe Text
  , budget :: Int64
  , revenue :: Int64
  , imdbId :: Maybe Text
  , productionCompanies :: [ProductionCompany]
  , productionCountries :: [ProductionCountry]
  , spokenLanguages :: [SpokenLanguage]
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON MovieDetail where
  parseJSON = withObject "MovieDetail" $ \o ->
    MovieDetail
      <$> o .: "id"
      <*> o .: "title"
      <*> o .: "original_title"
      <*> o .:? "overview" .!= ""
      <*> o .:? "tagline"
      <*> o .:? "poster_path"
      <*> o .:? "backdrop_path"
      <*> o .:? "release_date"
      <*> o .:? "runtime"
      <*> o .: "vote_average"
      <*> o .: "vote_count"
      <*> o .: "popularity"
      <*> o .:? "genres" .!= []
      <*> o .: "original_language"
      <*> o .:? "adult" .!= False
      <*> o .:? "video" .!= False
      <*> o .: "status"
      <*> o .:? "homepage"
      <*> o .:? "budget" .!= 0
      <*> o .:? "revenue" .!= 0
      <*> o .:? "imdb_id"
      <*> o .:? "production_companies" .!= []
      <*> o .:? "production_countries" .!= []
      <*> o .:? "spoken_languages" .!= []
