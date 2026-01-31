module Test.Tmdb.Types.CommonSpec (spec) where

import Data.Aeson (eitherDecode, encode)
import Data.ByteString.Lazy (ByteString)
import Network.Tmdb.Types.Common (MovieId (..), PaginatedResponse (..), TvShowId (..))
import Test.Hspec
import Web.HttpApiData (toUrlPiece)

spec :: Spec
spec = do
  describe "MovieId" $ do
    it "parses from JSON number" $ do
      let json :: ByteString
          json = "550"
      case eitherDecode json :: Either String MovieId of
        Left err -> expectationFailure err
        Right mid -> mid `shouldBe` MovieId 550

    it "encodes to JSON number" $ do
      encode (MovieId 550) `shouldBe` "550"

    it "converts to URL path piece" $ do
      toUrlPiece (MovieId 550) `shouldBe` "550"

  describe "TvShowId" $ do
    it "parses from JSON number" $ do
      let json :: ByteString
          json = "12345"
      case eitherDecode json :: Either String TvShowId of
        Left err -> expectationFailure err
        Right tid -> tid `shouldBe` TvShowId 12345

    it "encodes to JSON number" $ do
      encode (TvShowId 12345) `shouldBe` "12345"

    it "converts to URL path piece" $ do
      toUrlPiece (TvShowId 12345) `shouldBe` "12345"

  describe "PaginatedResponse" $ do
    it "parses complete JSON" $ do
      let json :: ByteString
          json =
            "{\
            \  \"page\": 1,\
            \  \"results\": [1, 2, 3],\
            \  \"total_pages\": 10,\
            \  \"total_results\": 100\
            \}"
      case eitherDecode json :: Either String (PaginatedResponse Int) of
        Left err -> expectationFailure err
        Right resp -> do
          resp.page `shouldBe` 1
          resp.results `shouldBe` [1, 2, 3]
          resp.totalPages `shouldBe` 10
          resp.totalResults `shouldBe` 100

    it "parses with empty results" $ do
      let json :: ByteString
          json =
            "{\
            \  \"page\": 1,\
            \  \"results\": [],\
            \  \"total_pages\": 0,\
            \  \"total_results\": 0\
            \}"
      case eitherDecode json :: Either String (PaginatedResponse Int) of
        Left err -> expectationFailure err
        Right resp -> do
          resp.page `shouldBe` 1
          resp.results `shouldBe` []
          resp.totalPages `shouldBe` 0
          resp.totalResults `shouldBe` 0

    it "fails on missing required fields" $ do
      let json :: ByteString
          json = "{\"page\": 1}"
      case eitherDecode json :: Either String (PaginatedResponse Int) of
        Left _ -> pure ()
        Right _ -> expectationFailure "Should fail on missing fields"
