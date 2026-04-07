# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What This Is

An R package (Golem-based Shiny app) for energy commodities analytics. The package builds a local versioned data snapshot from live APIs, then serves a dashboard that reads only from the snapshot — the app never calls external APIs at runtime.

## Commands

```sh
# Build data snapshot (requires EIA_API_KEY in .Renviron)
Rscript scripts/build-data.R

# Run R CMD check (requires snapshot to exist)
Rscript scripts/check.R

# Run tests only
Rscript -e 'devtools::test()'

# Run a single test file
Rscript -e 'testthat::test_file("tests/testthat/test-data-foundation.R")'

# Launch the Shiny dashboard
Rscript -e 'pkgload::load_all(); run_app()'

# Recompile C++ (Rcpp) after editing src/
Rscript -e 'pkgload::load_all()'
```

Bootstrap for fresh clone: copy `.Renviron.example` to `.Renviron`, set `EIA_API_KEY`, then `Rscript scripts/bootstrap.R && Rscript scripts/check-env.R`.

## Architecture

**Data pipeline** (`data-raw/` scripts, orchestrated by `R/data_foundation.R`):
- Fetches commodity futures curves (`RTL::dflong`), Treasury curves (`tidyquant::tq_get` via FRED), EIA inventory data (`RTL::eia2tidy_all`), and options chains
- Normalizes everything into Feather files under `inst/extdata/latest/` with a `manifest.json` and `checksums.tsv`
- `ea_build_latest_snapshot()` is the single orchestrator; individual `ea_build_*()` functions produce each dataset

**C++ layer** (`src/greeks.cpp`):
- Vectorised Black-76 options pricing engine exposed via Rcpp
- `black76_greeks_vectorised()` computes premium + 1st/2nd/3rd order greeks
- `R/options_calculations.R` bridges the Feather datasets to this engine

**Shiny dashboard** (Golem pattern):
- `R/run_app.R` → `R/app_ui.R` / `R/app_server.R`
- Each analytics view is a Shiny module in `R/mod_*.R` (overview, fundamentals, forward_curves, volatility, options_greeks, codynamics, seasonality, hedging, scenarios)
- `R/mod_global_filters.R` provides shared commodity/benchmark filter state consumed by all modules
- Custom CSS theme in `inst/app/www/energy-analytics.css`
- Many modules still render deterministic mock outputs; calculation layer is being wired in incrementally

**Canonical snapshot datasets** (8 Feather files):
`market_registry`, `commodity_curve_long`, `commodity_curve_wide`, `contract_expiry_metadata`, `ust_curve_long`, `eia_stocks_long`, `eia_storage_capacity_long`, `options_surface_long`

## Source Policy (Enforced by Tests)

Live data sources are strictly limited to:
- `RTL::dflong` — commodity prices
- `RTL::eia2tidy_all` — EIA data
- `tidyquant::tq_get(..., get = "economic.data")` — FRED
- `RTL::expiry_table` — reference metadata only

Forbidden: `RTL::eiaStocks`, `RTL::eiaStorageCap`, `RTL::futuresRef`, `dfwide`, `readxl`, `download.file`, `fredr::*`, `FRED_API_KEY`. The test suite audits all R and data-raw source files for violations.

## Key Conventions

- The `.Rprofile` patches `quantmod::getSymbols.FRED` to use `curl --http1.1` (FRED HTTP/2 workaround). This is intentional — do not remove it.
- `renv.lock` pins all package versions; use `renv::restore()` not `install.packages()`.
- RTL is pinned to a specific commit via `Remotes:` in DESCRIPTION.
- All `data-raw/` scripts are development helpers; the supported workflow goes through `scripts/`.
- Tests skip gracefully when no snapshot exists (`skip_if_no_snapshot`).
- Register new NSE column names in `R/globals.R` to avoid R CMD check notes.
