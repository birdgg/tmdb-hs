module Test.Tmdb.Types.ErrorSpec (spec) where

import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy (ByteString)
import Network.Tmdb.Types.Error
  ( ApiErrorResponse (..)
  , TmdbError (..)
  , isAuthError
  , isNetworkError
  , isNotFound
  , isRateLimited
  )
import Test.Hspec

spec :: Spec
spec = do
  describe "ApiErrorResponse" $ do
    it "parses complete error response" $ do
      let json :: ByteString
          json =
            "{\
            \  \"success\": false,\
            \  \"status_code\": 7,\
            \  \"status_message\": \"Invalid API key\"\
            \}"
      case eitherDecode json :: Either String ApiErrorResponse of
        Left err -> expectationFailure err
        Right resp -> do
          resp.statusCode `shouldBe` 7
          resp.statusMessage `shouldBe` "Invalid API key"
          resp.success `shouldBe` Just False

    it "parses without success field" $ do
      let json :: ByteString
          json =
            "{\
            \  \"status_code\": 34,\
            \  \"status_message\": \"The resource could not be found\"\
            \}"
      case eitherDecode json :: Either String ApiErrorResponse of
        Left err -> expectationFailure err
        Right resp -> do
          resp.statusCode `shouldBe` 34
          resp.statusMessage `shouldBe` "The resource could not be found"
          resp.success `shouldBe` Nothing

    it "fails on missing status_code" $ do
      let json :: ByteString
          json = "{\"status_message\": \"Error\"}"
      case eitherDecode json :: Either String ApiErrorResponse of
        Left _ -> pure ()
        Right _ -> expectationFailure "Should fail on missing status_code"

    it "fails on missing status_message" $ do
      let json :: ByteString
          json = "{\"status_code\": 7}"
      case eitherDecode json :: Either String ApiErrorResponse of
        Left _ -> pure ()
        Right _ -> expectationFailure "Should fail on missing status_message"

  describe "TmdbError predicates" $ do
    describe "isNotFound" $ do
      it "returns True for NotFoundError" $ do
        let err = NotFoundError "movie" "123"
        isNotFound err `shouldBe` True

      it "returns True for API error with status code 34" $ do
        let err = ApiError 404 34 "Not found"
        isNotFound err `shouldBe` True

      it "returns True for API error with status code 6" $ do
        let err = ApiError 404 6 "Invalid id"
        isNotFound err `shouldBe` True

      it "returns True for HTTP 404" $ do
        let err = HttpError 404 "Not Found"
        isNotFound err `shouldBe` True

      it "returns False for other errors" $ do
        let err = AuthError "Auth failed"
        isNotFound err `shouldBe` False

    describe "isAuthError" $ do
      it "returns True for AuthError" $ do
        let err = AuthError "Invalid API key"
        isAuthError err `shouldBe` True

      it "returns True for API error with status code 3" $ do
        let err = ApiError 401 3 "Auth failed"
        isAuthError err `shouldBe` True

      it "returns True for API error with status code 7" $ do
        let err = ApiError 401 7 "Invalid API key"
        isAuthError err `shouldBe` True

      it "returns True for HTTP 401" $ do
        let err = HttpError 401 "Unauthorized"
        isAuthError err `shouldBe` True

      it "returns False for other errors" $ do
        let err = NotFoundError "movie" "123"
        isAuthError err `shouldBe` False

    describe "isRateLimited" $ do
      it "returns True for RateLimitError" $ do
        let err = RateLimitError (Just 30)
        isRateLimited err `shouldBe` True

      it "returns True for API error with status code 25" $ do
        let err = ApiError 429 25 "Rate limited"
        isRateLimited err `shouldBe` True

      it "returns True for HTTP 429" $ do
        let err = HttpError 429 "Too Many Requests"
        isRateLimited err `shouldBe` True

      it "returns False for other errors" $ do
        let err = AuthError "Auth failed"
        isRateLimited err `shouldBe` False

    describe "isNetworkError" $ do
      it "returns True for ConnectionError" $ do
        let err = ConnectionError "Connection refused"
        isNetworkError err `shouldBe` True

      it "returns False for other errors" $ do
        let err = HttpError 500 "Server error"
        isNetworkError err `shouldBe` False

  describe "TmdbError Eq instance" $ do
    it "considers same errors equal" $ do
      let err1 = ApiError 400 7 "Invalid key"
          err2 = ApiError 400 7 "Invalid key"
      err1 `shouldBe` err2

    it "considers different errors not equal" $ do
      let err1 = ApiError 400 7 "Invalid key"
          err2 = ApiError 400 3 "Auth failed"
      err1 `shouldNotBe` err2

  describe "TmdbError Show instance" $ do
    it "shows ApiError" $ do
      let err = ApiError 400 7 "Invalid key"
      show err `shouldContain` "ApiError"
      show err `shouldContain` "400"
      show err `shouldContain` "7"

    it "shows NotFoundError" $ do
      let err = NotFoundError "movie" "123"
      show err `shouldContain` "NotFoundError"
      show err `shouldContain` "movie"

    it "shows AuthError" $ do
      let err = AuthError "Invalid API key"
      show err `shouldContain` "AuthError"
      show err `shouldContain` "Invalid API key"

    it "shows ConnectionError" $ do
      let err = ConnectionError "Connection refused"
      show err `shouldContain` "ConnectionError"
