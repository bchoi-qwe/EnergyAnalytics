# Contract Expiry Metadata

## What this is

`contract_expiry_metadata.feather` is a reference metadata artifact built from
`RTL::expiry_table`.

It is not live market data. It is static contract-calendar metadata.

## What it is for

Use this dataset when calculations need real contract dates rather than simple
curve-depth labels like `CL01`, `CL02`, and `CL03`.

The main use cases are:
- roll logic for continuous series construction
- contract-aware return windows
- prompt/deferred mapping to actual delivery months
- notice and delivery awareness for physical-market analytics
- hedge and carry calculations that depend on real contract timing

## When to implement it

This metadata becomes important once the project moves beyond generic curve
analytics and into contract-aware calculations.

Bring it into the calculation layer before building:
- rolled return series
- calendar-spread analytics tied to actual contract months
- delivery-sensitive hedge logic
- expiry-aware risk windows
- seasonality tied to named contract months instead of generic tenor buckets

## When you do not need it

You do not need this metadata for the earliest calculations that only depend on
curve depth and history from `RTL::dflong`, such as:
- basic forward-curve shape analysis
- volatility by tenor bucket
- simple co-dynamics by prompt/deferred depth
- early hedge-ratio prototypes using generic depth labels

## Why `futuresRef` is still excluded

`futuresRef` was not added back because it is not a strong fit as an
authoritative calculation input for this project:
- coverage is incomplete for the market set, especially Brent
- much of the content is descriptive text rather than structured fields
- it is less useful for calculation work than explicit expiry metadata

## Intended project rule

For this repo:
- `RTL::dflong` is the commodity price source
- `RTL::eia2tidy_all` is the EIA live data path
- `tidyquant::tq_get` is the FRED live data path
- `RTL::expiry_table` is allowed only as reference metadata
