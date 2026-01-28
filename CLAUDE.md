# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build Commands

```bash
# Build the library
cabal build

# Run GHCi with the library loaded
cabal repl

# Clean build artifacts
cabal clean
```

## Architecture

This is a Haskell client library for the TMDB API using servant-client for type-safe HTTP requests.

### Module Structure

```
Network.Tmdb           -- Main entry point, re-exports IO interface
Network.Tmdb.IO        -- High-level IO interface (TmdbClient, TmdbConfig)
Network.Tmdb.Client    -- Low-level ClientM functions
Network.Tmdb.API       -- Servant API type definitions (TmdbRoutes)
Network.Tmdb.Types     -- Type re-exports
```

### Adding New API Endpoints

1. **Types** (`Types/<Resource>.hs`): Define data types with manual `FromJSON` instances mapping snake_case JSON to camelCase Haskell fields
2. **API Routes** (`API/<Resource>.hs`): Define Servant routes using `NamedRoutes` pattern with `QueryParam' '[Required, Strict]` for required params
3. **Client** (`Client.hs`): Add wrapper function calling the generated client
4. **IO** (`IO.hs`): Add high-level function using `runClientM`
5. **Exports**: Update `Types.hs`, `API.hs`, `Tmdb.hs`, and `tmdb.cabal`

### Key Patterns

- Uses GHC2021 with `NoFieldSelectors`, `OverloadedRecordDot`, `DuplicateRecordFields`
- All API functions take `apiKey` and `language` as required parameters
- Optional JSON fields use `.:?` with `.!= defaultValue` for defaults
- Types derive `Show, Eq, Generic` via `deriving stock`
