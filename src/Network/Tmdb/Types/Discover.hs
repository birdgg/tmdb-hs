-- | Discover parameters for TMDB API
module Network.Tmdb.Types.Discover
  ( -- * Request Parameters
    DiscoverTvParams (..)
  )
where

import Data.Text (Text)
import GHC.Generics (Generic)

-- | Parameters for discover TV endpoint
data DiscoverTvParams = DiscoverTvParams
  { withGenres :: Maybe Text
  -- ^ Genre IDs (comma-separated, e.g. "16" for Animation)
  , withTextQuery :: Maybe Text
  -- ^ Text query filter
  }
  deriving stock (Show, Eq, Generic)
