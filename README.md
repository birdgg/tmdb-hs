# tmdb-hs

A pure Haskell client library for [TMDB (The Movie Database) API](https://www.themoviedb.org/documentation/api).

## Features

- Type-safe API bindings using [servant-client](https://hackage.haskell.org/package/servant-client)
- Comprehensive error handling with domain-specific error types
- Type-safe locale support using the [country](https://hackage.haskell.org/package/country) package
- Support for Movies, TV Shows, Seasons, Episodes, and Search

## Installation

Add to your `cabal` file:

```cabal
build-depends:
  tmdb
```

Or with Stack, add to `stack.yaml`:

```yaml
extra-deps:
  - git: https://github.com/birdgg/tmdb-hs.git
    commit: <commit-hash>
```

## Quick Start

```haskell
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Tmdb

main :: IO ()
main = do
  -- Create HTTP manager and API client
  manager <- newManager tlsManagerSettings
  let tmdb = mkTmdbClient (TmdbConfig "your-api-key" zhCN) manager

  -- Get movie details
  movieResult <- tmdb.getMovieDetail (MovieId 550)  -- Fight Club
  case movieResult of
    Right movie -> putStrLn $ "Title: " <> show movie.title
    Left err -> handleError err

  -- Search for TV shows
  tvResult <- tmdb.searchTv "Breaking Bad"
  case tvResult of
    Right shows -> mapM_ (print . (.name)) shows.results
    Left err -> handleError err

handleError :: TmdbError -> IO ()
handleError err
  | isAuthError err = putStrLn "Invalid API key!"
  | isNotFound err = putStrLn "Resource not found"
  | isRateLimited err = putStrLn "Rate limited, please wait"
  | otherwise = print err
```

## Error Handling

The library provides comprehensive error handling with the `TmdbError` type:

```haskell
data TmdbError
  = TmdbApiError        -- TMDB API returned a structured error
      { httpStatus :: Int
      , tmdbStatusCode :: Int64
      , tmdbStatusMessage :: Text
      }
  | TmdbNotFoundError   -- Resource not found (HTTP 404)
      { resourceType :: Text
      , resourceId :: Text
      }
  | TmdbAuthError       -- Authentication failed
      { authMessage :: Text
      }
  | TmdbRateLimitError  -- Rate limit exceeded (HTTP 429)
      { retryAfter :: Maybe Int
      }
  | TmdbHttpError       -- HTTP error without parseable body
      { httpStatus :: Int
      , httpBody :: Text
      }
  | TmdbDecodeError     -- JSON parsing failed
      { decodeMessage :: Text
      , responseBody :: Text
      }
  | TmdbConnectionError -- Network connectivity error
      { connectionMessage :: Text
      }
  | TmdbUnknownError    -- Unexpected error
      { unknownMessage :: Text
      }
```

### Error Predicates

Convenient functions for error checking:

```haskell
isNotFound    :: TmdbError -> Bool  -- 404 or TMDB status 34/6
isAuthError   :: TmdbError -> Bool  -- 401 or TMDB status 3/7
isRateLimited :: TmdbError -> Bool  -- 429 or TMDB status 25
isNetworkError :: TmdbError -> Bool -- Connection failures
```

### Converting from ClientError

If you need to work with raw servant-client errors:

```haskell
import Servant.Client (ClientError)
import Network.Tmdb (fromClientError)

-- Convert ClientError to TmdbError
handleClientError :: ClientError -> TmdbError
handleClientError = fromClientError
```

## API Coverage

### Movies

```haskell
-- Get movie details by ID
tmdb.getMovieDetail :: MovieId -> IO (Either TmdbError MovieDetail)
```

### TV Shows

```haskell
-- Search TV shows
tmdb.searchTv :: Text -> IO (Either TmdbError (PaginatedResponse TvShow))

-- Get TV show details
tmdb.getTvDetail :: TvShowId -> IO (Either TmdbError TvDetail)

-- Get season details
tmdb.getTvSeasonDetail :: TvShowId -> Int -> IO (Either TmdbError TvSeasonDetail)

-- Get episode details
tmdb.getTvEpisodeDetail :: TvShowId -> Int -> Int -> IO (Either TmdbError TvEpisodeDetail)
```

### Discovery and Search

```haskell
-- Discover TV shows with filters
tmdb.discoverTv :: DiscoverTvParams -> IO (Either TmdbError (PaginatedResponse TvShow))

-- Multi search (movies, TV shows, people)
tmdb.searchMulti :: Text -> IO (Either TmdbError (PaginatedResponse MultiSearchResult))
```

## Locale Support

The library uses type-safe locales with the `TmdbLocale` type:

```haskell
-- Pre-defined locales
enUS, enGB :: TmdbLocale  -- English
zhCN, zhTW :: TmdbLocale  -- Chinese
jaJP :: TmdbLocale        -- Japanese
koKR :: TmdbLocale        -- Korean
frFR :: TmdbLocale        -- French
deDE :: TmdbLocale        -- German
esES, esMX :: TmdbLocale  -- Spanish
ptBR, ptPT :: TmdbLocale  -- Portuguese
itIT :: TmdbLocale        -- Italian
ruRU :: TmdbLocale        -- Russian

-- Create custom locale
import Country (unitedKingdom)
myLocale = mkLocale "en" unitedKingdom  -- "en-GB"
```

## Image URLs

Helper functions for constructing image URLs:

```haskell
import Network.Tmdb

-- Get poster URL
let url = posterUrl W500 movie.posterPath
-- Result: "https://image.tmdb.org/t/p/w500/path.jpg"

-- Available sizes
posterUrl   :: PosterSize -> Maybe Text -> Maybe Text
backdropUrl :: BackdropSize -> Maybe Text -> Maybe Text
profileUrl  :: ProfileSize -> Maybe Text -> Maybe Text
logoUrl     :: LogoSize -> Maybe Text -> Maybe Text
stillUrl    :: StillSize -> Maybe Text -> Maybe Text
```

## Types

### Movie Types

- `Movie` - Basic movie info (from search/discover results)
- `MovieDetail` - Full movie details including genres, production info, budget, revenue
- `Genre`, `ProductionCompany`, `ProductionCountry`, `SpokenLanguage`

### TV Types

- `TvShow` - Basic TV show info
- `TvDetail` - Full TV show details
- `TvSeasonDetail` - Season details with episodes
- `TvEpisode`, `TvEpisodeDetail` - Episode info

### Common Types

- `MovieId`, `TvShowId` - Type-safe ID wrappers
- `PaginatedResponse a` - Wrapper for paginated API responses
- `MediaType` - `MediaMovie | MediaTv | MediaPerson`
- `MultiSearchResult` - Union type for multi-search results
- `TmdbError` - Domain-specific error type

## Configuration

```haskell
data TmdbConfig = TmdbConfig
  { apiKey :: Text       -- Your TMDB API key
  , locale :: TmdbLocale -- Locale for results (e.g., zhCN, enUS, jaJP)
  }
```

## Getting an API Key

1. Create an account at [themoviedb.org](https://www.themoviedb.org/)
2. Go to Settings > API
3. Request an API key

## License

MIT
