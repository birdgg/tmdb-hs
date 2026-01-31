-- | Error types for TMDB API
module Network.Tmdb.Types.Error
  ( -- * Error Types
    TmdbError (..)
  , TmdbApiErrorResponse (..)

    -- * Error Conversion
  , fromClientError

    -- * Error Predicates
  , isNotFound
  , isAuthError
  , isRateLimited
  , isNetworkError
  )
where

import Data.Aeson (FromJSON (..), eitherDecode, withObject, (.:), (.:?))
import Data.ByteString.Lazy (ByteString)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Network.HTTP.Types.Status (Status (..))
import Servant.Client (ClientError (..), ResponseF (..))

-- | TMDB API error response body
--
-- TMDB returns errors in this format:
--
-- @
-- {
--   "success": false,
--   "status_code": 7,
--   "status_message": "Invalid API key: You must be granted a valid key."
-- }
-- @
data TmdbApiErrorResponse = TmdbApiErrorResponse
  { statusCode :: Int64
  -- ^ TMDB-specific status code (not HTTP status)
  , statusMessage :: Text
  -- ^ Human-readable error message
  , success :: Maybe Bool
  -- ^ Optional success field (always false for errors)
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON TmdbApiErrorResponse where
  parseJSON = withObject "TmdbApiErrorResponse" $ \o ->
    TmdbApiErrorResponse
      <$> o .: "status_code"
      <*> o .: "status_message"
      <*> o .:? "success"

-- | Errors that can occur during TMDB API operations
data TmdbError
  = -- | TMDB API returned a structured error response
    -- Contains HTTP status, TMDB status code, and message
    TmdbApiError
      { httpStatus :: Int
      , tmdbStatusCode :: Int64
      , tmdbStatusMessage :: Text
      }
  | -- | Resource not found (HTTP 404)
    TmdbNotFoundError
      { resourceType :: Text
      , resourceId :: Text
      }
  | -- | Authentication failed (invalid API key, expired token, etc.)
    TmdbAuthError
      { authMessage :: Text
      }
  | -- | Rate limit exceeded (HTTP 429)
    TmdbRateLimitError
      { retryAfter :: Maybe Int
      }
  | -- | HTTP error without parseable TMDB error body
    TmdbHttpError
      { httpStatus :: Int
      , httpBody :: Text
      }
  | -- | Failed to decode JSON response
    TmdbDecodeError
      { decodeMessage :: Text
      , responseBody :: Text
      }
  | -- | Network connectivity error
    TmdbConnectionError
      { connectionMessage :: Text
      }
  | -- | Unexpected error (catch-all)
    TmdbUnknownError
      { unknownMessage :: Text
      }
  deriving stock (Show, Eq)

-- | Convert a servant-client 'ClientError' to 'TmdbError'
fromClientError :: ClientError -> TmdbError
fromClientError = \case
  FailureResponse _req resp ->
    parseFailureResponse resp
  DecodeFailure msg resp ->
    TmdbDecodeError
      { decodeMessage = msg
      , responseBody = truncateBody (responseBody resp)
      }
  UnsupportedContentType _mediaType resp ->
    TmdbHttpError
      { httpStatus = statusCode (responseStatusCode resp)
      , httpBody = "Unsupported content type"
      }
  InvalidContentTypeHeader resp ->
    TmdbHttpError
      { httpStatus = statusCode (responseStatusCode resp)
      , httpBody = "Invalid content type header"
      }
  ConnectionError exc ->
    TmdbConnectionError
      { connectionMessage = T.pack (show exc)
      }

-- | Parse a failure response into appropriate TmdbError
parseFailureResponse :: ResponseF ByteString -> TmdbError
parseFailureResponse resp =
  let status = statusCode (responseStatusCode resp)
      body = responseBody resp
   in case status of
        404 -> TmdbNotFoundError "resource" "unknown"
        429 -> TmdbRateLimitError Nothing
        401 -> parseAuthError body
        _ -> parseApiError status body

-- | Parse authentication error from response body
parseAuthError :: ByteString -> TmdbError
parseAuthError body =
  case eitherDecode body :: Either String TmdbApiErrorResponse of
    Right apiErr ->
      TmdbAuthError {authMessage = apiErr.statusMessage}
    Left _ ->
      TmdbAuthError {authMessage = "Authentication failed"}

-- | Parse API error from response body
parseApiError :: Int -> ByteString -> TmdbError
parseApiError status body =
  case eitherDecode body :: Either String TmdbApiErrorResponse of
    Right apiErr ->
      -- Check for specific TMDB status codes
      case apiErr.statusCode of
        -- Authentication errors
        3 -> TmdbAuthError {authMessage = apiErr.statusMessage}
        7 -> TmdbAuthError {authMessage = apiErr.statusMessage}
        -- Not found errors
        6 -> TmdbNotFoundError "id" "invalid"
        34 -> TmdbNotFoundError "resource" "not found"
        -- Rate limit
        25 -> TmdbRateLimitError Nothing
        -- Generic API error
        _ ->
          TmdbApiError
            { httpStatus = status
            , tmdbStatusCode = apiErr.statusCode
            , tmdbStatusMessage = apiErr.statusMessage
            }
    Left _ ->
      TmdbHttpError
        { httpStatus = status
        , httpBody = truncateBody body
        }

-- | Truncate response body for error messages
truncateBody :: ByteString -> Text
truncateBody body =
  let txt = T.pack (show body)
   in if T.length txt > 200
        then T.take 200 txt <> "..."
        else txt

-- | Check if error is a not-found error
isNotFound :: TmdbError -> Bool
isNotFound = \case
  TmdbNotFoundError {} -> True
  TmdbApiError {tmdbStatusCode = 34} -> True
  TmdbApiError {tmdbStatusCode = 6} -> True
  TmdbHttpError {httpStatus = 404} -> True
  _ -> False

-- | Check if error is an authentication error
isAuthError :: TmdbError -> Bool
isAuthError = \case
  TmdbAuthError {} -> True
  TmdbApiError {tmdbStatusCode = 3} -> True
  TmdbApiError {tmdbStatusCode = 7} -> True
  TmdbHttpError {httpStatus = 401} -> True
  _ -> False

-- | Check if error is a rate limit error
isRateLimited :: TmdbError -> Bool
isRateLimited = \case
  TmdbRateLimitError {} -> True
  TmdbApiError {tmdbStatusCode = 25} -> True
  TmdbHttpError {httpStatus = 429} -> True
  _ -> False

-- | Check if error is a network connectivity error
isNetworkError :: TmdbError -> Bool
isNetworkError = \case
  TmdbConnectionError {} -> True
  _ -> False
