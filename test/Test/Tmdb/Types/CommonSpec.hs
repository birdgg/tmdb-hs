module Test.Tmdb.Types.CommonSpec (spec) where

import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy (ByteString)
import Network.Tmdb.Types.Common (PaginatedResponse (..))
import Test.Hspec

spec :: Spec
spec = describe "PaginatedResponse" $ do
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
