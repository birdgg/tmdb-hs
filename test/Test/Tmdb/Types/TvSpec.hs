module Test.Tmdb.Types.TvSpec (spec) where

import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy (ByteString)
import Data.Time.Calendar (fromGregorian)
import Network.Tmdb.Types.Common (EpisodeId (..), GenreId (..), SeasonId (..), TvShowId (..))
import Network.Tmdb.Types.Tv
import Test.Hspec

spec :: Spec
spec = do
  describe "TvStatus" $ do
    it "parses all known statuses" $ do
      eitherDecode "\"Returning Series\"" `shouldBe` Right TvReturning
      eitherDecode "\"Planned\"" `shouldBe` Right TvPlanned
      eitherDecode "\"In Production\"" `shouldBe` Right TvInProduction
      eitherDecode "\"Ended\"" `shouldBe` Right TvEnded
      eitherDecode "\"Canceled\"" `shouldBe` Right TvCanceled
      eitherDecode "\"Pilot\"" `shouldBe` Right TvPilot

    it "parses unknown status as TvStatusUnknown" $ do
      eitherDecode "\"New Status\"" `shouldBe` Right (TvStatusUnknown "New Status")

  describe "TvShow" $ do
    it "parses complete JSON" $ do
      let json :: ByteString
          json =
            "{\
            \  \"id\": 12345,\
            \  \"name\": \"Test Show\",\
            \  \"original_name\": \"Test Show Original\",\
            \  \"overview\": \"A test show\",\
            \  \"poster_path\": \"/poster.jpg\",\
            \  \"backdrop_path\": \"/backdrop.jpg\",\
            \  \"first_air_date\": \"2023-01-15\",\
            \  \"vote_average\": 8.5,\
            \  \"vote_count\": 1000,\
            \  \"popularity\": 50.5,\
            \  \"genre_ids\": [18, 35],\
            \  \"origin_country\": [\"US\"],\
            \  \"original_language\": \"en\"\
            \}"
      case eitherDecode json :: Either String TvShow of
        Left err -> expectationFailure err
        Right tv -> do
          tv.id `shouldBe` TvShowId 12345
          tv.name `shouldBe` "Test Show"
          tv.originalName `shouldBe` "Test Show Original"
          tv.overview `shouldBe` "A test show"
          tv.posterPath `shouldBe` Just "/poster.jpg"
          tv.backdropPath `shouldBe` Just "/backdrop.jpg"
          tv.firstAirDate `shouldBe` Just (fromGregorian 2023 1 15)
          tv.voteAverage `shouldBe` 8.5
          tv.voteCount `shouldBe` 1000
          tv.popularity `shouldBe` 50.5
          tv.genreIds `shouldBe` [GenreId 18, GenreId 35]
          tv.originCountry `shouldBe` ["US"]
          tv.originalLanguage `shouldBe` "en"

    it "parses with optional fields missing" $ do
      let json :: ByteString
          json =
            "{\
            \  \"id\": 12345,\
            \  \"name\": \"Test Show\",\
            \  \"original_name\": \"Test Show Original\",\
            \  \"overview\": \"A test show\",\
            \  \"vote_average\": 8.5,\
            \  \"vote_count\": 1000,\
            \  \"popularity\": 50.5,\
            \  \"original_language\": \"en\"\
            \}"
      case eitherDecode json :: Either String TvShow of
        Left err -> expectationFailure err
        Right tv -> do
          tv.id `shouldBe` TvShowId 12345
          tv.posterPath `shouldBe` Nothing
          tv.backdropPath `shouldBe` Nothing
          tv.firstAirDate `shouldBe` Nothing
          tv.genreIds `shouldBe` []
          tv.originCountry `shouldBe` []

  describe "TvDetail" $ do
    it "parses complete JSON" $ do
      let json :: ByteString
          json =
            "{\
            \  \"id\": 12345,\
            \  \"name\": \"Test Show\",\
            \  \"original_name\": \"Test Show Original\",\
            \  \"overview\": \"A detailed test show\",\
            \  \"poster_path\": \"/poster.jpg\",\
            \  \"backdrop_path\": \"/backdrop.jpg\",\
            \  \"first_air_date\": \"2023-01-15\",\
            \  \"last_air_date\": \"2023-12-20\",\
            \  \"vote_average\": 8.5,\
            \  \"vote_count\": 1000,\
            \  \"popularity\": 50.5,\
            \  \"origin_country\": [\"US\"],\
            \  \"original_language\": \"en\",\
            \  \"status\": \"Returning Series\",\
            \  \"number_of_seasons\": 3,\
            \  \"number_of_episodes\": 30,\
            \  \"homepage\": \"https://example.com\",\
            \  \"seasons\": [\
            \    {\
            \      \"season_number\": 1,\
            \      \"name\": \"Season 1\",\
            \      \"episode_count\": 10,\
            \      \"air_date\": \"2023-01-15\"\
            \    }\
            \  ]\
            \}"
      case eitherDecode json :: Either String TvDetail of
        Left err -> expectationFailure err
        Right tv -> do
          tv.id `shouldBe` TvShowId 12345
          tv.status `shouldBe` TvReturning
          tv.numberOfSeasons `shouldBe` 3
          tv.numberOfEpisodes `shouldBe` 30
          tv.homepage `shouldBe` Just "https://example.com"
          length tv.seasons `shouldBe` 1

    it "parses with optional fields missing" $ do
      let json :: ByteString
          json =
            "{\
            \  \"id\": 12345,\
            \  \"name\": \"Test Show\",\
            \  \"original_name\": \"Test Show Original\",\
            \  \"overview\": \"A detailed test show\",\
            \  \"vote_average\": 8.5,\
            \  \"vote_count\": 1000,\
            \  \"popularity\": 50.5,\
            \  \"original_language\": \"en\",\
            \  \"status\": \"Ended\",\
            \  \"number_of_seasons\": 1,\
            \  \"number_of_episodes\": 10\
            \}"
      case eitherDecode json :: Either String TvDetail of
        Left err -> expectationFailure err
        Right tv -> do
          tv.posterPath `shouldBe` Nothing
          tv.firstAirDate `shouldBe` Nothing
          tv.lastAirDate `shouldBe` Nothing
          tv.homepage `shouldBe` Nothing
          tv.seasons `shouldBe` []
          tv.originCountry `shouldBe` []

  describe "TvSeasonSummary" $ do
    it "parses complete JSON" $ do
      let json :: ByteString
          json =
            "{\
            \  \"season_number\": 1,\
            \  \"name\": \"Season 1\",\
            \  \"episode_count\": 10,\
            \  \"air_date\": \"2023-01-15\"\
            \}"
      case eitherDecode json :: Either String TvSeasonSummary of
        Left err -> expectationFailure err
        Right season -> do
          season.seasonNumber `shouldBe` 1
          season.name `shouldBe` "Season 1"
          season.episodeCount `shouldBe` 10
          season.airDate `shouldBe` Just (fromGregorian 2023 1 15)

    it "parses with optional air_date missing" $ do
      let json :: ByteString
          json =
            "{\
            \  \"season_number\": 0,\
            \  \"name\": \"Specials\",\
            \  \"episode_count\": 5\
            \}"
      case eitherDecode json :: Either String TvSeasonSummary of
        Left err -> expectationFailure err
        Right season -> do
          season.seasonNumber `shouldBe` 0
          season.airDate `shouldBe` Nothing

  describe "TvSeasonDetail" $ do
    it "parses complete JSON" $ do
      let json :: ByteString
          json =
            "{\
            \  \"id\": 54321,\
            \  \"name\": \"Season 1\",\
            \  \"overview\": \"First season\",\
            \  \"poster_path\": \"/season1.jpg\",\
            \  \"season_number\": 1,\
            \  \"air_date\": \"2023-01-15\",\
            \  \"vote_average\": 8.0,\
            \  \"vote_count\": 500,\
            \  \"episodes\": []\
            \}"
      case eitherDecode json :: Either String TvSeasonDetail of
        Left err -> expectationFailure err
        Right season -> do
          season.id `shouldBe` SeasonId 54321
          season.name `shouldBe` "Season 1"
          season.overview `shouldBe` "First season"
          season.seasonNumber `shouldBe` 1

    it "parses with minimal fields" $ do
      let json :: ByteString
          json =
            "{\
            \  \"id\": 54321,\
            \  \"season_number\": 1\
            \}"
      case eitherDecode json :: Either String TvSeasonDetail of
        Left err -> expectationFailure err
        Right season -> do
          season.id `shouldBe` SeasonId 54321
          season.name `shouldBe` ""
          season.overview `shouldBe` ""
          season.posterPath `shouldBe` Nothing
          season.airDate `shouldBe` Nothing
          season.voteAverage `shouldBe` 0
          season.voteCount `shouldBe` 0
          season.episodes `shouldBe` []

  describe "TvEpisode" $ do
    it "parses complete JSON" $ do
      let json :: ByteString
          json =
            "{\
            \  \"id\": 11111,\
            \  \"name\": \"Pilot\",\
            \  \"overview\": \"First episode\",\
            \  \"air_date\": \"2023-01-15\",\
            \  \"episode_number\": 1,\
            \  \"season_number\": 1,\
            \  \"still_path\": \"/still.jpg\",\
            \  \"vote_average\": 8.5,\
            \  \"vote_count\": 200,\
            \  \"runtime\": 45,\
            \  \"production_code\": \"S01E01\"\
            \}"
      case eitherDecode json :: Either String TvEpisode of
        Left err -> expectationFailure err
        Right ep -> do
          ep.id `shouldBe` EpisodeId 11111
          ep.name `shouldBe` "Pilot"
          ep.overview `shouldBe` "First episode"
          ep.episodeNumber `shouldBe` 1
          ep.seasonNumber `shouldBe` 1
          ep.runtime `shouldBe` Just 45
          ep.productionCode `shouldBe` Just "S01E01"

    it "parses with minimal fields" $ do
      let json :: ByteString
          json =
            "{\
            \  \"id\": 11111,\
            \  \"name\": \"Pilot\",\
            \  \"episode_number\": 1,\
            \  \"season_number\": 1\
            \}"
      case eitherDecode json :: Either String TvEpisode of
        Left err -> expectationFailure err
        Right ep -> do
          ep.overview `shouldBe` ""
          ep.airDate `shouldBe` Nothing
          ep.stillPath `shouldBe` Nothing
          ep.voteAverage `shouldBe` 0
          ep.voteCount `shouldBe` 0
          ep.runtime `shouldBe` Nothing
          ep.productionCode `shouldBe` Nothing

  describe "TvEpisodeDetail" $ do
    it "parses complete JSON" $ do
      let json :: ByteString
          json =
            "{\
            \  \"id\": 11111,\
            \  \"show_id\": 12345,\
            \  \"name\": \"Pilot\",\
            \  \"overview\": \"First episode\",\
            \  \"air_date\": \"2023-01-15\",\
            \  \"episode_number\": 1,\
            \  \"season_number\": 1,\
            \  \"still_path\": \"/still.jpg\",\
            \  \"vote_average\": 8.5,\
            \  \"vote_count\": 200,\
            \  \"runtime\": 45,\
            \  \"production_code\": \"S01E01\"\
            \}"
      case eitherDecode json :: Either String TvEpisodeDetail of
        Left err -> expectationFailure err
        Right ep -> do
          ep.id `shouldBe` EpisodeId 11111
          ep.showId `shouldBe` TvShowId 12345
          ep.name `shouldBe` "Pilot"
          ep.episodeNumber `shouldBe` 1
          ep.seasonNumber `shouldBe` 1

    it "parses with minimal fields" $ do
      let json :: ByteString
          json =
            "{\
            \  \"id\": 11111,\
            \  \"show_id\": 12345,\
            \  \"name\": \"Pilot\",\
            \  \"episode_number\": 1,\
            \  \"season_number\": 1\
            \}"
      case eitherDecode json :: Either String TvEpisodeDetail of
        Left err -> expectationFailure err
        Right ep -> do
          ep.overview `shouldBe` ""
          ep.airDate `shouldBe` Nothing
          ep.stillPath `shouldBe` Nothing
          ep.voteAverage `shouldBe` 0
          ep.voteCount `shouldBe` 0
          ep.runtime `shouldBe` Nothing
          ep.productionCode `shouldBe` Nothing
