-- | Image configuration for TMDB API
module Network.Tmdb.Types.Image
  ( -- * Base URL
    imageBaseUrl

    -- * Image Sizes
  , PosterSize (..)
  , BackdropSize (..)
  , ProfileSize (..)
  , LogoSize (..)
  , StillSize (..)

    -- * URL Construction
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

-- | Available backdrop image sizes
data BackdropSize
  = BackdropW300
  | BackdropW780
  | BackdropW1280
  | BackdropOriginal
  deriving stock (Show, Eq, Ord, Enum, Bounded)

-- | Available profile image sizes
data ProfileSize
  = ProfileW45
  | ProfileW185
  | ProfileH632
  | ProfileOriginal
  deriving stock (Show, Eq, Ord, Enum, Bounded)

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

-- | Available still image sizes (for TV episodes)
data StillSize
  = StillW92
  | StillW185
  | StillW300
  | StillOriginal
  deriving stock (Show, Eq, Ord, Enum, Bounded)

-- | Convert poster size to URL path segment
posterSizeToText :: PosterSize -> Text
posterSizeToText = \case
  PosterW92 -> "w92"
  PosterW154 -> "w154"
  PosterW185 -> "w185"
  PosterW342 -> "w342"
  PosterW500 -> "w500"
  PosterW780 -> "w780"
  PosterOriginal -> "original"

-- | Convert backdrop size to URL path segment
backdropSizeToText :: BackdropSize -> Text
backdropSizeToText = \case
  BackdropW300 -> "w300"
  BackdropW780 -> "w780"
  BackdropW1280 -> "w1280"
  BackdropOriginal -> "original"

-- | Convert profile size to URL path segment
profileSizeToText :: ProfileSize -> Text
profileSizeToText = \case
  ProfileW45 -> "w45"
  ProfileW185 -> "w185"
  ProfileH632 -> "h632"
  ProfileOriginal -> "original"

-- | Convert logo size to URL path segment
logoSizeToText :: LogoSize -> Text
logoSizeToText = \case
  LogoW45 -> "w45"
  LogoW92 -> "w92"
  LogoW154 -> "w154"
  LogoW185 -> "w185"
  LogoW300 -> "w300"
  LogoW500 -> "w500"
  LogoOriginal -> "original"

-- | Convert still size to URL path segment
stillSizeToText :: StillSize -> Text
stillSizeToText = \case
  StillW92 -> "w92"
  StillW185 -> "w185"
  StillW300 -> "w300"
  StillOriginal -> "original"

-- | Construct full URL for a poster image
--
-- @
-- posterUrl PosterW500 "/abc123.jpg"
-- -- "https://image.tmdb.org/t/p/w500/abc123.jpg"
-- @
posterUrl :: PosterSize -> Text -> Text
posterUrl size path = imageBaseUrl <> "/" <> posterSizeToText size <> ensureLeadingSlash path

-- | Construct full URL for a backdrop image
backdropUrl :: BackdropSize -> Text -> Text
backdropUrl size path = imageBaseUrl <> "/" <> backdropSizeToText size <> ensureLeadingSlash path

-- | Construct full URL for a profile image
profileUrl :: ProfileSize -> Text -> Text
profileUrl size path = imageBaseUrl <> "/" <> profileSizeToText size <> ensureLeadingSlash path

-- | Construct full URL for a logo image
logoUrl :: LogoSize -> Text -> Text
logoUrl size path = imageBaseUrl <> "/" <> logoSizeToText size <> ensureLeadingSlash path

-- | Construct full URL for a still image (TV episode)
stillUrl :: StillSize -> Text -> Text
stillUrl size path = imageBaseUrl <> "/" <> stillSizeToText size <> ensureLeadingSlash path

-- | Ensure path has leading slash
ensureLeadingSlash :: Text -> Text
ensureLeadingSlash path
  | T.null path = path
  | T.head path == '/' = path
  | otherwise = "/" <> path
