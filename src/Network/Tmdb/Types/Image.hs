-- | Image configuration for TMDB API
module Network.Tmdb.Types.Image
  ( -- * Base URL
    imageBaseUrl

    -- * Image Size
  , ImageSize (..)

    -- * Size Types
  , PosterSize (..)
  , BackdropSize (..)
  , ProfileSize (..)
  , LogoSize (..)
  , StillSize (..)

    -- * URL Construction
  , imageUrl
  , posterUrl
  , backdropUrl
  , profileUrl
  , logoUrl
  , stillUrl
  )
where

import Data.Text (Text)
import qualified Data.Text as T

-- | TMDB image CDN base URL
--
-- All TMDB images are served from this base URL with a size prefix.
-- Example: imageBaseUrl <> "/w500" <> posterPath
imageBaseUrl :: Text
imageBaseUrl = "https://image.tmdb.org/t/p"

-- | Type class for TMDB image sizes
class ImageSize a where
  sizeToText :: a -> Text

-- | Available poster image sizes
data PosterSize
  = PosterW92
  | PosterW154
  | PosterW185
  | PosterW342
  | PosterW500
  | PosterW780
  | PosterOriginal
  deriving stock (Show, Eq, Ord, Enum, Bounded)

instance ImageSize PosterSize where
  sizeToText = \case
    PosterW92 -> "w92"
    PosterW154 -> "w154"
    PosterW185 -> "w185"
    PosterW342 -> "w342"
    PosterW500 -> "w500"
    PosterW780 -> "w780"
    PosterOriginal -> "original"

-- | Available backdrop image sizes
data BackdropSize
  = BackdropW300
  | BackdropW780
  | BackdropW1280
  | BackdropOriginal
  deriving stock (Show, Eq, Ord, Enum, Bounded)

instance ImageSize BackdropSize where
  sizeToText = \case
    BackdropW300 -> "w300"
    BackdropW780 -> "w780"
    BackdropW1280 -> "w1280"
    BackdropOriginal -> "original"

-- | Available profile image sizes
data ProfileSize
  = ProfileW45
  | ProfileW185
  | ProfileH632
  | ProfileOriginal
  deriving stock (Show, Eq, Ord, Enum, Bounded)

instance ImageSize ProfileSize where
  sizeToText = \case
    ProfileW45 -> "w45"
    ProfileW185 -> "w185"
    ProfileH632 -> "h632"
    ProfileOriginal -> "original"

-- | Available logo image sizes
data LogoSize
  = LogoW45
  | LogoW92
  | LogoW154
  | LogoW185
  | LogoW300
  | LogoW500
  | LogoOriginal
  deriving stock (Show, Eq, Ord, Enum, Bounded)

instance ImageSize LogoSize where
  sizeToText = \case
    LogoW45 -> "w45"
    LogoW92 -> "w92"
    LogoW154 -> "w154"
    LogoW185 -> "w185"
    LogoW300 -> "w300"
    LogoW500 -> "w500"
    LogoOriginal -> "original"

-- | Available still image sizes (for TV episodes)
data StillSize
  = StillW92
  | StillW185
  | StillW300
  | StillOriginal
  deriving stock (Show, Eq, Ord, Enum, Bounded)

instance ImageSize StillSize where
  sizeToText = \case
    StillW92 -> "w92"
    StillW185 -> "w185"
    StillW300 -> "w300"
    StillOriginal -> "original"

-- | Construct full URL for any TMDB image
--
-- @
-- imageUrl PosterW500 "/abc123.jpg"
-- -- "https://image.tmdb.org/t/p/w500/abc123.jpg"
-- @
imageUrl :: (ImageSize s) => s -> Text -> Text
imageUrl size path = imageBaseUrl <> "/" <> sizeToText size <> ensureLeadingSlash path

-- | Construct full URL for a poster image
posterUrl :: PosterSize -> Text -> Text
posterUrl = imageUrl

-- | Construct full URL for a backdrop image
backdropUrl :: BackdropSize -> Text -> Text
backdropUrl = imageUrl

-- | Construct full URL for a profile image
profileUrl :: ProfileSize -> Text -> Text
profileUrl = imageUrl

-- | Construct full URL for a logo image
logoUrl :: LogoSize -> Text -> Text
logoUrl = imageUrl

-- | Construct full URL for a still image (TV episode)
stillUrl :: StillSize -> Text -> Text
stillUrl = imageUrl

-- | Ensure path has leading slash
ensureLeadingSlash :: Text -> Text
ensureLeadingSlash path
  | T.null path = path
  | T.head path == '/' = path
  | otherwise = "/" <> path
