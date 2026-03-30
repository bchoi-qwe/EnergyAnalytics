# FRED Shell Fix

## What this is

The repo root contains a `.Rprofile` file that activates the project bootstrap
and patches `quantmod::getSymbols.FRED` at startup.

This is a shell-environment fix, not a data-model decision.

## Why it exists

In this shell, the default `quantmod` FRED path fails because it uses
`curl::curl()` against the FRED CSV endpoint and negotiates a broken HTTP/2
path.

The same FRED endpoint works with the system `curl` binary when forced to
HTTP/1.1.

## What the patch does

The startup patch replaces the internal FRED fetch inside `quantmod` with a
call to:

`curl --http1.1 -fsSL https://fred.stlouisfed.org/graph/fredgraph.csv?id=...`

That keeps the external interface unchanged:
- `tidyquant::tq_get(..., get = "economic.data")` still works
- repo code still uses `tidyquant::tq_get`

## Scope

This fix is project-local because it is wired through the repo `.Rprofile`
and the shared `scripts/_startup.R` bootstrap.

It affects R sessions started from this repo directory, including `Rscript`
runs used for the canonical `scripts/` entrypoints.

## When to remove it

Remove this patch only when `tidyquant::tq_get()` works in this shell without
the startup override.
