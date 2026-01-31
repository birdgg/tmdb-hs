module Test.Tmdb.Types.MovieSpec (spec) where

import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy (ByteString)
import Network.Tmdb.Types.Movie
import Test.Hspec

spec :: Spec
spec = do
  describe "Genre" $ do
    it "parses complete JSON" $ do
      let json :: ByteString
          json = "{\"id\": 28, \"name\": \"Action\"}"
      case eitherDecode json :: Either String Genre of
        Left err -> expectationFailure err
        Right genre -> do
          genre.id `shouldBe` 28
          genre.name `shouldBe` "Action"

    it "fails on missing fields" $ do
      let json :: ByteString
          json = "{\"id\": 28}"
      case eitherDecode json :: Either String Genre of
        Left _ -> pure ()
        Right _ -> expectationFailure "Should fail on missing name"

  describe "ProductionCompany" $ do
    it "parses complete JSON" $ do
      let json :: ByteString
          json =
            "{\
            \  \"id\": 1,\
            \  \"name\": \"Warner Bros\",\
            \  \"logo_path\": \"/logo.png\",\
            \  \"origin_country\": \"US\"\
            \}"
      case eitherDecode json :: Either String ProductionCompany of
        Left err -> expectationFailure err
        Right company -> do
          company.id `shouldBe` 1
          company.name `shouldBe` "Warner Bros"
          company.logoPath `shouldBe` Just "/logo.png"
          company.originCountry `shouldBe` "US"

    it "parses with optional fields missing" $ do
      let json :: ByteString
          json = "{\"id\": 1, \"name\": \"Warner Bros\"}"
      case eitherDecode json :: Either String ProductionCompany of
        Left err -> expectationFailure err
        Right company -> do
          company.logoPath `shouldBe` Nothing
          company.originCountry `shouldBe` ""

  describe "ProductionCountry" $ do
    it "parses complete JSON" $ do
      let json :: ByteString
          json = "{\"iso_3166_1\": \"US\", \"name\": \"United States\"}"
      case eitherDecode json :: Either String ProductionCountry of
        Left err -> expectationFailure err
        Right country -> do
          country.iso31661 `shouldBe` "US"
          country.name `shouldBe` "United States"

  describe "SpokenLanguage" $ do
    it "parses complete JSON" $ do
      let json :: ByteString
          json =
            "{\
            \  \"iso_639_1\": \"en\",\
            \  \"name\": \"English\",\
            \  \"english_name\": \"English\"\
            \}"
      case eitherDecode json :: Either String SpokenLanguage of
        Left err -> expectationFailure err
        Right lang -> do
          lang.iso6391 `shouldBe` "en"
          lang.name `shouldBe` "English"
          lang.englishName `shouldBe` "English"

    it "parses with optional english_name missing" $ do
      let json :: ByteString
          json = "{\"iso_639_1\": \"ja\", \"name\": \"Japanese\"}"
      case eitherDecode json :: Either String SpokenLanguage of
        Left err -> expectationFailure err
        Right lang -> do
          lang.iso6391 `shouldBe` "ja"
          lang.englishName `shouldBe` ""

  describe "Movie" $ do
    it "parses complete JSON" $ do
      let json :: ByteString
          json =
            "{\
            \  \"id\": 550,\
            \  \"title\": \"Fight Club\",\
            \  \"original_title\": \"Fight Club\",\
            \  \"overview\": \"A depressed man...\",\
            \  \"poster_path\": \"/poster.jpg\",\
            \  \"backdrop_path\": \"/backdrop.jpg\",\
            \  \"release_date\": \"1999-10-15\",\
            \  \"vote_average\": 8.4,\
            \  \"vote_count\": 25000,\
            \  \"popularity\": 60.5,\
            \  \"genre_ids\": [18, 53],\
            \  \"original_language\": \"en\",\
            \  \"adult\": false,\
            \  \"video\": false\
            \}"
      case eitherDecode json :: Either String Movie of
        Left err -> expectationFailure err
        Right movie -> do
          movie.id `shouldBe` 550
          movie.title `shouldBe` "Fight Club"
          movie.originalTitle `shouldBe` "Fight Club"
          movie.overview `shouldBe` "A depressed man..."
          movie.posterPath `shouldBe` Just "/poster.jpg"
          movie.backdropPath `shouldBe` Just "/backdrop.jpg"
          movie.releaseDate `shouldBe` Just "1999-10-15"
          movie.voteAverage `shouldBe` 8.4
          movie.voteCount `shouldBe` 25000
          movie.popularity `shouldBe` 60.5
          movie.genreIds `shouldBe` [18, 53]
          movie.originalLanguage `shouldBe` "en"
          movie.adult `shouldBe` False
          movie.video `shouldBe` False

    it "parses with optional fields missing" $ do
      let json :: ByteString
          json =
            "{\
            \  \"id\": 550,\
            \  \"title\": \"Fight Club\",\
            \  \"original_title\": \"Fight Club\",\
            \  \"vote_average\": 8.4,\
            \  \"vote_count\": 25000,\
            \  \"popularity\": 60.5,\
            \  \"original_language\": \"en\"\
            \}"
      case eitherDecode json :: Either String Movie of
        Left err -> expectationFailure err
        Right movie -> do
          movie.overview `shouldBe` ""
          movie.posterPath `shouldBe` Nothing
          movie.backdropPath `shouldBe` Nothing
          movie.releaseDate `shouldBe` Nothing
          movie.genreIds `shouldBe` []
          movie.adult `shouldBe` False
          movie.video `shouldBe` False

  describe "MovieDetail" $ do
    it "parses complete JSON" $ do
      let json :: ByteString
          json =
            "{\
            \  \"id\": 550,\
            \  \"title\": \"Fight Club\",\
            \  \"original_title\": \"Fight Club\",\
            \  \"overview\": \"A depressed man...\",\
            \  \"tagline\": \"Mischief. Mayhem. Soap.\",\
            \  \"poster_path\": \"/poster.jpg\",\
            \  \"backdrop_path\": \"/backdrop.jpg\",\
            \  \"release_date\": \"1999-10-15\",\
            \  \"runtime\": 139,\
            \  \"vote_average\": 8.4,\
            \  \"vote_count\": 25000,\
            \  \"popularity\": 60.5,\
            \  \"genres\": [{\"id\": 18, \"name\": \"Drama\"}],\
            \  \"original_language\": \"en\",\
            \  \"adult\": false,\
            \  \"video\": false,\
            \  \"status\": \"Released\",\
            \  \"homepage\": \"https://example.com\",\
            \  \"budget\": 63000000,\
            \  \"revenue\": 100853753,\
            \  \"imdb_id\": \"tt0137523\",\
            \  \"production_companies\": [],\
            \  \"production_countries\": [],\
            \  \"spoken_languages\": []\
            \}"
      case eitherDecode json :: Either String MovieDetail of
        Left err -> expectationFailure err
        Right movie -> do
          movie.id `shouldBe` 550
          movie.title `shouldBe` "Fight Club"
          movie.tagline `shouldBe` Just "Mischief. Mayhem. Soap."
          movie.runtime `shouldBe` Just 139
          movie.status `shouldBe` "Released"
          movie.homepage `shouldBe` Just "https://example.com"
          movie.budget `shouldBe` 63000000
          movie.revenue `shouldBe` 100853753
          movie.imdbId `shouldBe` Just "tt0137523"
          length movie.genres `shouldBe` 1

    it "parses with optional fields missing" $ do
      let json :: ByteString
          json =
            "{\
            \  \"id\": 550,\
            \  \"title\": \"Fight Club\",\
            \  \"original_title\": \"Fight Club\",\
            \  \"vote_average\": 8.4,\
            \  \"vote_count\": 25000,\
            \  \"popularity\": 60.5,\
            \  \"original_language\": \"en\",\
            \  \"status\": \"Released\"\
            \}"
      case eitherDecode json :: Either String MovieDetail of
        Left err -> expectationFailure err
        Right movie -> do
          movie.overview `shouldBe` ""
          movie.tagline `shouldBe` Nothing
          movie.posterPath `shouldBe` Nothing
          movie.runtime `shouldBe` Nothing
          movie.homepage `shouldBe` Nothing
          movie.budget `shouldBe` 0
          movie.revenue `shouldBe` 0
          movie.imdbId `shouldBe` Nothing
          movie.genres `shouldBe` []
          movie.productionCompanies `shouldBe` []
          movie.productionCountries `shouldBe` []
          movie.spokenLanguages `shouldBe` []
          movie.adult `shouldBe` False
          movie.video `shouldBe` False
