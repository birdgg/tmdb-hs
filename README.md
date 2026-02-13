# tmdb-hs

A pure Haskell client library for [TMDB (The Movie Database) API](https://www.themoviedb.org/documentation/api).

## Features

- Type-safe API bindings using [servant-client](https://hackage.haskell.org/package/servant-client)
- Two-layer error handling separating transport errors from TMDB API errors
- Type-safe locale support with `TmdbLocale` newtype
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

handleError :: TmdbClientError -> IO ()
handleError = \case
  TmdbError apiErr -> case apiErr.httpStatus of
    401 -> putStrLn $ "Auth error: " <> show apiErr.statusMessage
    404 -> putStrLn "Resource not found"
    429 -> putStrLn "Rate limited, please wait"
    _   -> putStrLn $ "API error: " <> show apiErr.statusMessage
  ServantError clientErr ->
    putStrLn $ "Transport error: " <> show clientErr
```

## Error Handling

The library uses a two-layer error type that separates transport-level errors
(network, decoding) from TMDB API domain errors:

```haskell
data TmdbClientError
  = ServantError ClientError   -- Network, connection, or HTTP-level errors
  | TmdbError TmdbApiError     -- Structured errors from TMDB API

data TmdbApiError = TmdbApiError
  { httpStatus :: Int       -- HTTP status code (401, 404, 429, etc.)
  , statusCode :: Int64     -- TMDB-specific status code
  , statusMessage :: Text   -- Human-readable error message
  }
```

### Handling by HTTP status

```haskell
case result of
  Right value -> use value
  Left (TmdbError apiErr) | apiErr.httpStatus == 404 -> handleNotFound
  Left (TmdbError apiErr) | apiErr.httpStatus == 401 -> handleAuth
  Left (TmdbError apiErr) | apiErr.httpStatus == 429 -> handleRateLimit
  Left (TmdbError apiErr) -> handleApiError apiErr
  Left (ServantError err) -> handleTransportError err
```

### Handling by TMDB status code

```haskell
case result of
  Left (TmdbError apiErr) -> case apiErr.statusCode of
    7  -> putStrLn "Invalid API key"
    34 -> putStrLn "Resource not found"
    25 -> putStrLn "Rate limit exceeded"
    _  -> print apiErr.statusMessage
  Left (ServantError _) -> putStrLn "Network error"
  Right value -> use value
```

### Converting from ClientError

If you need to work with raw servant-client errors:

```haskell
import Servant.Client (ClientError)
import Network.Tmdb.Types.Error (fromClientError)

-- Convert ClientError to TmdbClientError
convert :: ClientError -> TmdbClientError
convert = fromClientError
```

## API Coverage

### Movies

```haskell
-- Get movie details by ID
tmdb.getMovieDetail :: MovieId -> IO (Either TmdbClientError MovieDetail)
```

### TV Shows

```haskell
-- Search TV shows
tmdb.searchTv :: Text -> IO (Either TmdbClientError (PaginatedResponse TvShow))

-- Get TV show details
tmdb.getTvDetail :: TvShowId -> IO (Either TmdbClientError TvDetail)

-- Get season details
tmdb.getTvSeasonDetail :: TvShowId -> Int -> IO (Either TmdbClientError TvSeasonDetail)

-- Get episode details
tmdb.getTvEpisodeDetail :: TvShowId -> Int -> Int -> IO (Either TmdbClientError TvEpisodeDetail)
```

### Discovery and Search

```haskell
-- Discover TV shows with filters
tmdb.discoverTv :: DiscoverTvParams -> IO (Either TmdbClientError (PaginatedResponse TvShow))

-- Multi search (movies, TV shows, people)
tmdb.searchMulti :: Text -> IO (Either TmdbClientError (PaginatedResponse MultiSearchResult))
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
myLocale = TmdbLocale "en-GB"
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
- `TmdbClientError` - Two-layer error type (transport + domain)
- `TmdbApiError` - TMDB API domain error

## Configuration

```haskell
data TmdbConfig = TmdbConfig
  { apiKey :: Text       -- Your TMDB API key
  , language :: TmdbLocale -- Locale for results (e.g., zhCN, enUS, jaJP)
  }
```

## Getting an API Key

1. Create an account at [themoviedb.org](https://www.themoviedb.org/)
2. Go to Settings > API
3. Request an API key

## License

MIT
