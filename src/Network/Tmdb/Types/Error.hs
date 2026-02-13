-- | Error types for TMDB API
--
-- Uses a two-layer error pattern separating transport-level errors
-- (servant 'ClientError') from TMDB API domain errors ('TmdbApiError').
module Network.Tmdb.Types.Error
  ( -- * Error Types
    TmdbClientError (..)
  , TmdbApiError (..)
  , ApiErrorResponse (..)

    -- * Error Conversion
  , fromClientError
  )
where

import Control.DeepSeq (NFData (..))
import Control.Exception (Exception)
import Data.Aeson (FromJSON (..), eitherDecode, withObject, (.:), (.:?))
import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Types.Status (Status (..))
import Servant.Client
  ( ClientError (..)
  , ResponseF (..)
  )

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
data ApiErrorResponse = ApiErrorResponse
  { statusCode :: Int64
  -- ^ TMDB-specific status code (not HTTP status)
  , statusMessage :: Text
  -- ^ Human-readable error message
  , success :: Maybe Bool
  -- ^ Optional success field (always false for errors)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

instance FromJSON ApiErrorResponse where
  parseJSON = withObject "ApiErrorResponse" $ \o ->
    ApiErrorResponse
      <$> o .: "status_code"
      <*> o .: "status_message"
      <*> o .:? "success"

-- | TMDB API domain errors
--
-- Represents structured errors parsed from TMDB API failure responses.
-- Check 'httpStatus' for HTTP-level classification (404, 401, 429, etc.)
-- or 'statusCode' for TMDB-specific error codes.
data TmdbApiError = TmdbApiError
  { httpStatus :: Int
  -- ^ HTTP status code from the response
  , statusCode :: Int64
  -- ^ TMDB-specific status code
  , statusMessage :: Text
  -- ^ Human-readable error message from TMDB
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

-- | Top-level error type for TMDB client operations
--
-- Separates transport-layer errors from TMDB API domain errors:
--
-- * 'ServantError' — network, connection, decoding, or HTTP-level failures
-- * 'TmdbError' — structured errors returned by the TMDB API
data TmdbClientError
  = -- | Transport-layer error from servant-client
    ServantError ClientError
  | -- | Domain-level error from TMDB API (parsed from response body)
    TmdbError TmdbApiError
  deriving stock (Show, Generic)

instance NFData TmdbClientError where
  rnf (ServantError _) = ()
  rnf (TmdbError e) = rnf e

instance Exception TmdbClientError

-- | Convert a servant-client 'ClientError' to 'TmdbClientError'
--
-- For 'FailureResponse' errors, attempts to parse the response body as a
-- TMDB 'ApiErrorResponse'. If parsing succeeds, returns 'TmdbError';
-- otherwise wraps the original error as 'ServantError'.
fromClientError :: ClientError -> TmdbClientError
fromClientError err@(FailureResponse _req resp) =
  case eitherDecode (responseBody resp) :: Either String ApiErrorResponse of
    Right apiErr ->
      TmdbError
        TmdbApiError
          { httpStatus = statusCode (responseStatusCode resp)
          , statusCode = apiErr.statusCode
          , statusMessage = apiErr.statusMessage
          }
    Left _ -> ServantError err
fromClientError err = ServantError err
