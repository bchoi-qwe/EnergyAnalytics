# EnergyAnalytics

EnergyAnalytics is an R package / Golem project with a local canonical data
snapshot under `inst/extdata/latest`. Development is intentionally package-first:
all tooling should enter through the repo root, use the pinned `renv` library,
and call the same `scripts/` entrypoints in local shells, RStudio, CI, and
later Docker.

The data layer is live and package-based. All per-module analytics views are
wired to real calculation layers that read from the packaged canonical snapshot.
This eliminates all runtime dependencies on external APIs.

## Source policy

The live source boundary is fixed:

- `RTL::dflong` for baseline commodity reference prices
- `RTL::getPrices(...)` / ZEMA for listed commodity contract prices and option quotes
- `RTL::eia2tidy_all` for EIA
- `tidyquant::tq_get(..., get = "economic.data")` for FRED
- `RTL::expiry_table` only as reference metadata

No fallback data-source logic should be added outside that boundary.
Legacy local option-chain CSV inputs are not part of the supported data path. Optional ZEMA contract lists remain allowed only to extend unsupported feeds such as ICE/Endex options.

## Local setup

1. Copy `.Renviron.example` to `.Renviron`.
2. Set `EIA_API_KEY`.
3. Set `RTL_IUSER` and `RTL_IPASSWORD`. Snapshot builds require ZEMA authentication for listed contract prices and exchange options.
4. Copy `data-raw/zema_options_contracts.example.csv` to `data-raw/zema_options_contracts.csv` only when you need to extend the supported auto-derived CME option universe with explicit feed-specific contracts such as ICE/Endex options.
5. Run `Rscript scripts/bootstrap.R`.
6. Run `Rscript scripts/check-env.R`.

The repo `.Rprofile` activates the project bootstrap on entry. That preserves
the project-local FRED workaround that patches `quantmod` to fetch FRED through
`curl --http1.1` while still keeping repo code on `tidyquant::tq_get()`.

## Canonical workflow

Build the local snapshot and bundle:

```sh
Rscript scripts/build-data.R
```

Run the package check workflow against the current snapshot:

```sh
Rscript scripts/check.R
```

These are the canonical entrypoints. The `data-raw/` scripts remain as thin
development helpers, but the supported repo workflow is through `scripts/`.

## CI and Docker alignment

Use the same non-interactive entrypoints in CI:

```sh
Rscript scripts/bootstrap.R
Rscript scripts/check-env.R
Rscript scripts/build-data.R
Rscript scripts/check.R
```

`scripts/bootstrap.R` is lockfile-first. If `renv.lock` is present it restores
with `renv`; otherwise it installs from `DESCRIPTION` with `pak`. That keeps
local shells, CI, and Docker usable even when the repo is being iterated before
a new lockfile is committed.

The deployed Shiny app reads the bundled snapshot under `inst/extdata/latest`,
so container builds do not require `EIA_API_KEY`, `RTL_IUSER`, or
`RTL_IPASSWORD`. Those secrets are only needed when rebuilding the snapshot.
The Docker build context excludes `.Renviron`, so local credentials are not
baked into the image.

Build and run the container locally:

```sh
docker build -t energyanalytics .
docker run --rm -p 8080:8080 energyanalytics
```

The GitHub Actions workflow at `.github/workflows/docker.yml` builds the image
on pull requests, smoke-tests it by curling the app root, and publishes it to
GHCR plus Docker Hub on pushes to `main` and version tags.

For Docker Hub publishing, set:

- Repository variable `DOCKERHUB_USERNAME`
- Repository secret `DOCKERHUB_TOKEN`
