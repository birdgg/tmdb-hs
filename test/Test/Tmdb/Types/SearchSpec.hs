module Test.Tmdb.Types.SearchSpec (spec) where

import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy (ByteString)
import Network.Tmdb.Types.Search
import Test.Hspec

spec :: Spec
spec = do
  describe "MediaType" $ do
    it "parses movie" $ do
      let json :: ByteString
          json = "\"movie\""
      case eitherDecode json :: Either String MediaType of
        Left err -> expectationFailure err
        Right mt -> mt `shouldBe` MediaMovie

    it "parses tv" $ do
      let json :: ByteString
          json = "\"tv\""
      case eitherDecode json :: Either String MediaType of
        Left err -> expectationFailure err
        Right mt -> mt `shouldBe` MediaTv

    it "parses person" $ do
      let json :: ByteString
          json = "\"person\""
      case eitherDecode json :: Either String MediaType of
        Left err -> expectationFailure err
        Right mt -> mt `shouldBe` MediaPerson

    it "fails on unknown media type" $ do
      let json :: ByteString
          json = "\"unknown\""
      case eitherDecode json :: Either String MediaType of
        Left _ -> pure ()
        Right _ -> expectationFailure "Should fail on unknown media type"

  describe "MultiSearchResult" $ do
    it "parses movie result" $ do
      let json :: ByteString
          json =
            "{\
            \  \"id\": 550,\
            \  \"media_type\": \"movie\",\
            \  \"title\": \"Fight Club\",\
            \  \"original_title\": \"Fight Club\",\
            \  \"poster_path\": \"/poster.jpg\",\
            \  \"release_date\": \"1999-10-15\"\
            \}"
      case eitherDecode json :: Either String MultiSearchResult of
        Left err -> expectationFailure err
        Right result -> do
          result.id `shouldBe` 550
          result.mediaType `shouldBe` MediaMovie
          result.title `shouldBe` Just "Fight Club"
          result.originalTitle `shouldBe` Just "Fight Club"
          result.name `shouldBe` Nothing
          result.originalName `shouldBe` Nothing
          result.posterPath `shouldBe` Just "/poster.jpg"
          result.releaseDate `shouldBe` Just "1999-10-15"
          result.firstAirDate `shouldBe` Nothing

    it "parses tv result" $ do
      let json :: ByteString
          json =
            "{\
            \  \"id\": 12345,\
            \  \"media_type\": \"tv\",\
            \  \"name\": \"Breaking Bad\",\
            \  \"original_name\": \"Breaking Bad\",\
            \  \"poster_path\": \"/poster.jpg\",\
            \  \"first_air_date\": \"2008-01-20\"\
            \}"
      case eitherDecode json :: Either String MultiSearchResult of
        Left err -> expectationFailure err
        Right result -> do
          result.id `shouldBe` 12345
          result.mediaType `shouldBe` MediaTv
          result.name `shouldBe` Just "Breaking Bad"
          result.originalName `shouldBe` Just "Breaking Bad"
          result.title `shouldBe` Nothing
          result.originalTitle `shouldBe` Nothing
          result.posterPath `shouldBe` Just "/poster.jpg"
          result.firstAirDate `shouldBe` Just "2008-01-20"
          result.releaseDate `shouldBe` Nothing

    it "parses person result" $ do
      let json :: ByteString
          json =
            "{\
            \  \"id\": 287,\
            \  \"media_type\": \"person\",\
            \  \"name\": \"Brad Pitt\"\
            \}"
      case eitherDecode json :: Either String MultiSearchResult of
        Left err -> expectationFailure err
        Right result -> do
          result.id `shouldBe` 287
          result.mediaType `shouldBe` MediaPerson
          result.name `shouldBe` Just "Brad Pitt"

    it "parses with minimal fields" $ do
      let json :: ByteString
          json =
            "{\
            \  \"id\": 550,\
            \  \"media_type\": \"movie\"\
            \}"
      case eitherDecode json :: Either String MultiSearchResult of
        Left err -> expectationFailure err
        Right result -> do
          result.id `shouldBe` 550
          result.mediaType `shouldBe` MediaMovie
          result.name `shouldBe` Nothing
          result.title `shouldBe` Nothing
          result.posterPath `shouldBe` Nothing
