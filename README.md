# tmdb-hs

A pure Haskell client library for [TMDB (The Movie Database) API](https://www.themoviedb.org/documentation/api).

## Features

- Type-safe API bindings using [servant-client](https://hackage.haskell.org/package/servant-client)
- High-level IO interface with automatic HTTP client management
- Low-level ClientM interface for advanced use cases
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
import Network.Tmdb

main :: IO ()
main = do
  -- Create a client with your API key
  client <- newTmdbClient (TmdbConfig "your-api-key" "zh-CN")

  -- Get movie details
  movieResult <- getMovieDetail client 550  -- Fight Club
  case movieResult of
    Right movie -> putStrLn $ "Title: " <> show movie.title
    Left err -> print err

  -- Search for TV shows
  tvResult <- searchTv client "Breaking Bad"
  case tvResult of
    Right shows -> mapM_ (print . (.name)) shows.results
    Left err -> print err
```

## API Coverage

### Movies

```haskell
-- Get movie details by ID
getMovieDetail :: TmdbClient -> Int64 -> IO (Either ClientError MovieDetail)
```

### TV Shows

```haskell
-- Search TV shows
searchTv :: TmdbClient -> Text -> IO (Either ClientError (PaginatedResponse TvShow))

-- Get TV show details
getTvDetail :: TmdbClient -> Int64 -> IO (Either ClientError TvDetail)

-- Get season details
getTvSeasonDetail :: TmdbClient -> Int64 -> Int -> IO (Either ClientError TvSeasonDetail)

-- Get episode details
getTvEpisodeDetail :: TmdbClient -> Int64 -> Int -> Int -> IO (Either ClientError TvEpisodeDetail)
```

### Discovery & Search

```haskell
-- Discover TV shows with filters
discoverTv :: TmdbClient -> DiscoverTvParams -> IO (Either ClientError (PaginatedResponse TvShow))

-- Multi search (movies, TV shows, people)
searchMulti :: TmdbClient -> Text -> IO (Either ClientError (PaginatedResponse MultiSearchResult))
```

## Low-Level API

For more control, use the `Network.Tmdb.Client` module directly:

```haskell
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Network.Tmdb.Client as Tmdb
import Servant.Client (mkClientEnv, runClientM)

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  let env = mkClientEnv manager Tmdb.tmdbBaseUrl
  result <- runClientM (Tmdb.getMovieDetail "your-api-key" "en-US" 550) env
  print result
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

- `PaginatedResponse a` - Wrapper for paginated API responses
- `MediaType` - `MediaMovie | MediaTv | MediaPerson`
- `MultiSearchResult` - Union type for multi-search results

## Configuration

```haskell
data TmdbConfig = TmdbConfig
  { apiKey :: Text    -- Your TMDB API key
  , language :: Text  -- Language code (e.g., "zh-CN", "en-US", "ja-JP")
  }
```

## Getting an API Key

1. Create an account at [themoviedb.org](https://www.themoviedb.org/)
2. Go to Settings > API
3. Request an API key

## License

MIT
