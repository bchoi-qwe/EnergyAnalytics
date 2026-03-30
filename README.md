# EnergyAnalytics

EnergyAnalytics is an R package / Golem project with a local canonical data
snapshot under `inst/extdata/latest`. Development is intentionally package-first:
all tooling should enter through the repo root, use the pinned `renv` library,
and call the same `scripts/` entrypoints in local shells, RStudio, CI, and
later Docker.

The data layer is live and package-based. The current dashboard is still a UI
shell: it reads packaged metadata where needed, but the per-module analytics
views are still deterministic mock outputs until the calculation layer is wired
into those modules.

## Source policy

The live source boundary is fixed:

- `RTL::dflong` for commodity prices
- `RTL::eia2tidy_all` for EIA
- `tidyquant::tq_get(..., get = "economic.data")` for FRED
- `RTL::expiry_table` only as reference metadata

No fallback data-source logic should be added outside that boundary.

## Local setup

1. Copy `.Renviron.example` to `.Renviron`.
2. Set `EIA_API_KEY`.
3. Run `Rscript scripts/bootstrap.R`.
4. Run `Rscript scripts/check-env.R`.

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

Use the same sequence in non-interactive environments:

```sh
Rscript scripts/bootstrap.R
Rscript scripts/check-env.R
Rscript scripts/build-data.R
Rscript scripts/check.R
```

Inject `EIA_API_KEY` and, if GitHub rate limits matter for `RTL`, `GITHUB_PAT`
through environment variables rather than hard-coding them in image or repo
files. `renv.lock` is the package version contract for local, CI, and Docker
restores.
