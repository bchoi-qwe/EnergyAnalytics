# Calculation Layer Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace all `ea_mock_*` functions with real calculation layers that read from the canonical Feather snapshot and return plotly-ready dataframes.

**Architecture:** One `calc_*.R` file per module, each exporting an `ea_calc_*()` function that takes the standardized `filters` list and returns a named list of tidy dataframes. Shared helpers live in `calc_utils.R`. Heavy math uses Rcpp (existing Black-76 engine) and `slider` for rolling stats. RTL functions used where verified working.

**Tech Stack:** R, dplyr, tidyr, purrr, slider, broom, PerformanceAnalytics, RTL (fitOU, garch, rolladjust, simGBM, simOU, spreadOption), Rcpp (existing greeks engine)

---

## Task 0: Add New Dependencies to DESCRIPTION

**Files:**
- Modify: `DESCRIPTION`

- [ ] **Step 1: Add new Imports to DESCRIPTION**

Add these packages to the `Imports:` section in `DESCRIPTION`, maintaining alphabetical order:

```
Imports:
    arrow,
    broom,
    bslib,
    cli,
    digest,
    dplyr,
    glue,
    golem,
    htmltools,
    jsonlite,
    lubridate,
    moments,
    PerformanceAnalytics,
    plotly,
    purrr,
    reactable,
    Rcpp,
    readr,
    RTL,
    scales,
    shiny,
    shinycssloaders,
    shinyWidgets,
    slider,
    stringr,
    tibble,
    tidyr,
    tidyquant,
    tseries
```

- [ ] **Step 2: Add new global variables to `R/globals.R`**

Append to the existing `utils::globalVariables()` call in `R/globals.R`:

```r
utils::globalVariables(c(
  ".data",
  "First.Delivery",
  "First.Notice",
  "Last.Delivery",
  "Last.Trade",
  "Month",
  "Month.Letter",
  "Year",
  "cmdty",
  "contract_month",
  "contract_year",
  "curve_point",
  "curve_point_num",
  "dflong",
  "expiry_table",
  "first_notice",
  "frequency",
  "last_trade",
  "location",
  "market",
  "market_type",
  "n",
  "name",
  "price",
  "product",
  "raw_unit",
  "series",
  "source_family",
  "source_series",
  "tick.prefix",
  "ticker",
  "unit",
  "value",
  "cp_num",
  "days_to_expiry",
  "forward",
  "iv",
  "moneyness",
  "strike",
  "vanna",
  "charm",
  "gamma",
  "vega",
  "speed",
  "zomma",
  "avg_5yr",
  "avg_return",
  "beta",
  "capacity",
  "change",
  "change_pct",
  "comparison",
  "corr_normal",
  "corr_stress",
  "correlation",
  "current",
  "current_val",
  "days_from_peak",
  "deviation",
  "deviation_pct",
  "drawdown_pct",
  "garch_vol",
  "half_life",
  "hedged_vol",
  "hit_rate",
  "indexed_value",
  "label",
  "last_trade",
  "level",
  "log_return",
  "market_x",
  "market_y",
  "max_5yr",
  "mean_val",
  "min_5yr",
  "mu",
  "new_price",
  "ou_fit",
  "pair",
  "payoff",
  "percentile",
  "period",
  "pnl_impact",
  "r_squared",
  "realized",
  "realized_vol",
  "realized_vol_20d",
  "residual",
  "returns",
  "rolling_mean",
  "rolling_sd",
  "shock_pct",
  "sigma",
  "sim_id",
  "slope",
  "snapshot_label",
  "source_label",
  "spread",
  "spread_label",
  "spread_level",
  "std",
  "std_error",
  "stocks",
  "structure_label",
  "tenor",
  "theta",
  "unhedged_vol",
  "utilization_pct",
  "vol_percentile",
  "vol_reduction_pct",
  "week",
  "week_num",
  "window",
  "year",
  "zscore"
))
```

- [ ] **Step 3: Verify package loads**

Run: `Rscript -e 'pkgload::load_all()'`
Expected: No errors.

- [ ] **Step 4: Commit**

```bash
git add DESCRIPTION R/globals.R
git commit -m "chore: add slider, broom, PerformanceAnalytics, moments, tseries to Imports"
```

---

## Task 1: calc_utils.R — Shared Helpers

**Files:**
- Create: `R/calc_utils.R`
- Test: `tests/testthat/test-calc-utils.R`

- [ ] **Step 1: Write the failing tests**

Create `tests/testthat/test-calc-utils.R`:

```r
test_that("ea_latest_snapshot_date returns a Date", {
  skip_if_no_snapshot()
  result <- ea_latest_snapshot_date()
  expect_s3_class(result, "Date")
  expect_true(!is.na(result))
})

test_that("ea_load_filtered_curves filters by market and expiry range", {
  skip_if_no_snapshot()
  filters <- list(
    commodities = c("CL", "BRN"),
    expiry_range = c(1, 6),
    date_range = NULL
  )
  result <- ea_load_filtered_curves(filters)
  expect_s3_class(result, "data.frame")
  expect_true(all(result$market %in% c("CL", "BRN")))
  expect_true(all(result$curve_point_num >= 1 & result$curve_point_num <= 6))
  expect_gt(nrow(result), 0)
})

test_that("ea_load_filtered_curves filters by date_range", {
  skip_if_no_snapshot()
  filters <- list(
    commodities = "CL",
    expiry_range = c(1, 12),
    date_range = c(as.Date("2025-01-01"), as.Date("2025-06-30"))
  )
  result <- ea_load_filtered_curves(filters)
  expect_true(all(result$date >= as.Date("2025-01-01")))
  expect_true(all(result$date <= as.Date("2025-06-30")))
})

test_that("ea_log_returns computes correct log returns", {
  skip_if_no_snapshot()
  filters <- list(commodities = "CL", expiry_range = c(1, 1), date_range = NULL)
  curves <- ea_load_filtered_curves(filters)
  result <- ea_log_returns(curves)
  expect_true("log_return" %in% names(result))
  expect_type(result$log_return, "double")
  # First row per group should be NA (no lag)
  first_row <- result[result$date == min(result$date) & result$market == "CL", ]
  expect_true(is.na(first_row$log_return[1]))
})

test_that("ea_curve_to_wide pivots correctly", {
  skip_if_no_snapshot()
  filters <- list(commodities = "CL", expiry_range = c(1, 3), date_range = NULL)
  curves <- ea_load_filtered_curves(filters)
  result <- ea_curve_to_wide(curves)
  expect_true("date" %in% names(result))
  expect_true("market" %in% names(result))
  # Should have columns for each curve_point
  expect_true("01" %in% names(result) || "1" %in% names(result))
})
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `Rscript -e 'testthat::test_file("tests/testthat/test-calc-utils.R")'`
Expected: FAIL — functions not found.

- [ ] **Step 3: Write the implementation**

Create `R/calc_utils.R`:

```r
# ---- Shared Calculation Utilities ----
# Common helpers used by all calc_*.R modules.

ea_latest_snapshot_date <- function() {
  curves <- ea_load_dataset("commodity_curve_long")
  max(curves$date, na.rm = TRUE)
}

ea_load_filtered_curves <- function(filters) {
  curves <- ea_load_dataset("commodity_curve_long")
  markets <- ea_coalesce(filters$commodities, c("CL", "BRN", "RB"))
  expiry_lo <- ea_coalesce(filters$expiry_range[1], 1L)
  expiry_hi <- ea_coalesce(filters$expiry_range[2], 12L)

  out <- curves |>
    dplyr::filter(
      .data$market %in% markets,
      .data$curve_point_num >= expiry_lo,
      .data$curve_point_num <= expiry_hi
    )

  if (!is.null(filters$date_range) && length(filters$date_range) == 2L) {
    out <- out |>
      dplyr::filter(
        .data$date >= as.Date(filters$date_range[1]),
        .data$date <= as.Date(filters$date_range[2])
      )
  }

  out |> dplyr::arrange(.data$market, .data$curve_point_num, .data$date)
}

ea_log_returns <- function(curves_long) {
  curves_long |>
    dplyr::arrange(.data$market, .data$curve_point, .data$date) |>
    dplyr::group_by(.data$market, .data$curve_point) |>
    dplyr::mutate(log_return = log(.data$value / dplyr::lag(.data$value))) |>
    dplyr::ungroup()
}

ea_curve_to_wide <- function(curves_long) {
  curves_long |>
    dplyr::select(.data$date, .data$market, .data$curve_point, .data$value) |>
    tidyr::pivot_wider(names_from = .data$curve_point, values_from = .data$value)
}
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `Rscript -e 'testthat::test_file("tests/testthat/test-calc-utils.R")'`
Expected: All PASS.

- [ ] **Step 5: Commit**

```bash
git add R/calc_utils.R tests/testthat/test-calc-utils.R
git commit -m "feat: add calc_utils.R shared helpers for calculation layer"
```

---

## Task 2: calc_forward_curves.R — Term Structure

**Files:**
- Create: `R/calc_forward_curves.R`
- Test: `tests/testthat/test-calc-forward-curves.R`

- [ ] **Step 1: Write the failing tests**

Create `tests/testthat/test-calc-forward-curves.R`:

```r
test_that("ea_calc_forward_curves returns all expected outputs", {
  skip_if_no_snapshot()
  filters <- list(
    commodities = c("CL", "BRN"),
    comparison_commodity = "NG",
    expiry_range = c(1, 12),
    date_range = NULL,
    rolling_window = "63D"
  )
  result <- ea_calc_forward_curves(filters)

  expect_type(result, "list")
  expect_true(all(c(
    "curve_snapshot", "curve_history", "curve_change_heatmap",
    "calendar_spreads", "curve_shape_metrics"
  ) %in% names(result)))
})

test_that("curve_snapshot has correct shape", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("CL"), expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_forward_curves(filters)
  cs <- result$curve_snapshot

  expect_s3_class(cs, "data.frame")
  expect_true(all(c("market", "curve_point", "curve_point_num", "price", "label") %in% names(cs)))
  expect_true(all(cs$market == "CL"))
  expect_gt(nrow(cs), 0)
})

test_that("curve_history returns multiple snapshots", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("CL"), expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_forward_curves(filters)
  ch <- result$curve_history

  expect_s3_class(ch, "data.frame")
  expect_true("snapshot_label" %in% names(ch))
  expect_gt(length(unique(ch$snapshot_label)), 1)
})

test_that("calendar_spreads computes M1-M2 spreads", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("CL"), expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_forward_curves(filters)
  sp <- result$calendar_spreads

  expect_s3_class(sp, "data.frame")
  expect_true(all(c("date", "market", "spread_label", "value") %in% names(sp)))
  expect_gt(nrow(sp), 0)
})

test_that("curve_shape_metrics labels contango or backwardation", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("CL", "NG"), expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_forward_curves(filters)
  csm <- result$curve_shape_metrics

  expect_s3_class(csm, "data.frame")
  expect_true(all(c("market", "slope", "curvature", "structure_label") %in% names(csm)))
  expect_true(all(csm$structure_label %in% c("Contango", "Backwardation", "Flat")))
})
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `Rscript -e 'testthat::test_file("tests/testthat/test-calc-forward-curves.R")'`
Expected: FAIL — `ea_calc_forward_curves` not found.

- [ ] **Step 3: Write the implementation**

Create `R/calc_forward_curves.R`:

```r
# ---- Term Structure Calculation Layer ----
# Produces forward curve analytics from the canonical snapshot.

ea_calc_forward_curves <- function(filters) {
  catalog <- ea_market_catalog()
  curves <- ea_load_filtered_curves(filters)
  markets <- unique(curves$market)
  labels <- stats::setNames(catalog$label, catalog$market)
  latest_date <- max(curves$date, na.rm = TRUE)

  # --- curve_snapshot: current forward curve ---
  curve_snapshot <- curves |>
    dplyr::filter(.data$date == latest_date) |>
    dplyr::mutate(
      price = .data$value,
      label = labels[.data$market]
    ) |>
    dplyr::select(.data$market, .data$curve_point, .data$curve_point_num, .data$price, .data$label)

  # --- curve_history: historical curve snapshots ---
  snapshot_offsets <- c("Latest" = 0, "1W Ago" = 7, "1M Ago" = 30, "3M Ago" = 90)
  primary <- markets[1]
  primary_curves <- curves |> dplyr::filter(.data$market == primary)
  available_dates <- sort(unique(primary_curves$date), decreasing = TRUE)

  curve_history <- purrr::map_dfr(names(snapshot_offsets), function(lbl) {
    target <- latest_date - snapshot_offsets[[lbl]]
    # Find nearest available date
    idx <- which.min(abs(as.numeric(available_dates - target)))
    snap_date <- available_dates[idx]

    primary_curves |>
      dplyr::filter(.data$date == snap_date) |>
      dplyr::transmute(
        snapshot_label = lbl,
        curve_point_num = .data$curve_point_num,
        price = .data$value
      )
  })

  # --- curve_change_heatmap: daily changes by tenor ---
  primary_recent <- primary_curves |>
    dplyr::filter(.data$date >= latest_date - 60) |>
    dplyr::arrange(.data$curve_point_num, .data$date) |>
    dplyr::group_by(.data$curve_point_num) |>
    dplyr::mutate(change = .data$value - dplyr::lag(.data$value)) |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(.data$change)) |>
    dplyr::select(.data$date, .data$curve_point_num, .data$change)

  # --- calendar_spreads: M1-M2, M1-M3 timeseries ---
  calendar_spreads <- purrr::map_dfr(markets, function(mkt) {
    mkt_wide <- curves |>
      dplyr::filter(.data$market == mkt) |>
      ea_curve_to_wide()

    spread_pairs <- list(
      "M1-M2" = c("01", "02"),
      "M1-M3" = c("01", "03"),
      "M1-M6" = c("01", "06"),
      "M1-M12" = c("01", "12")
    )

    purrr::map_dfr(names(spread_pairs), function(sp_label) {
      cols <- spread_pairs[[sp_label]]
      if (!all(cols %in% names(mkt_wide))) return(tibble::tibble())

      tibble::tibble(
        date = mkt_wide$date,
        market = mkt,
        spread_label = sp_label,
        value = mkt_wide[[cols[1]]] - mkt_wide[[cols[2]]]
      ) |> dplyr::filter(!is.na(.data$value))
    })
  })

  # --- curve_shape_metrics ---
  curve_shape_metrics <- curve_snapshot |>
    dplyr::group_by(.data$market) |>
    dplyr::summarise(
      slope = dplyr::last(.data$price) - dplyr::first(.data$price),
      curvature = {
        mid <- .data$price[ceiling(dplyr::n() / 2)]
        avg_ends <- (dplyr::first(.data$price) + dplyr::last(.data$price)) / 2
        mid - avg_ends
      },
      .groups = "drop"
    ) |>
    dplyr::mutate(
      structure_label = dplyr::case_when(
        .data$slope > 0.01 ~ "Contango",
        .data$slope < -0.01 ~ "Backwardation",
        TRUE ~ "Flat"
      )
    )

  list(
    curve_snapshot = curve_snapshot,
    curve_history = curve_history,
    curve_change_heatmap = primary_recent,
    calendar_spreads = calendar_spreads,
    curve_shape_metrics = curve_shape_metrics
  )
}
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `Rscript -e 'testthat::test_file("tests/testthat/test-calc-forward-curves.R")'`
Expected: All PASS.

- [ ] **Step 5: Commit**

```bash
git add R/calc_forward_curves.R tests/testthat/test-calc-forward-curves.R
git commit -m "feat: add calc_forward_curves.R — term structure calculations"
```

---

## Task 3: calc_fundamentals.R — Physical Market

**Files:**
- Create: `R/calc_fundamentals.R`
- Test: `tests/testthat/test-calc-fundamentals.R`

- [ ] **Step 1: Write the failing tests**

Create `tests/testthat/test-calc-fundamentals.R`:

```r
test_that("ea_calc_fundamentals returns all expected outputs", {
  skip_if_no_snapshot()
  filters <- list(
    commodities = c("CL", "NG"),
    date_range = NULL,
    expiry_range = c(1, 12)
  )
  result <- ea_calc_fundamentals(filters)

  expect_type(result, "list")
  expect_true(all(c(
    "stocks_timeseries", "stocks_seasonal", "stocks_deviation",
    "storage_capacity_util"
  ) %in% names(result)))
})

test_that("stocks_timeseries returns EIA data", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("CL"), date_range = NULL, expiry_range = c(1, 12))
  result <- ea_calc_fundamentals(filters)
  st <- result$stocks_timeseries

  expect_s3_class(st, "data.frame")
  expect_true(all(c("date", "product", "location", "value", "unit") %in% names(st)))
  expect_gt(nrow(st), 0)
})

test_that("stocks_seasonal produces 5-year bands", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("CL"), date_range = NULL, expiry_range = c(1, 12))
  result <- ea_calc_fundamentals(filters)
  ss <- result$stocks_seasonal

  expect_s3_class(ss, "data.frame")
  expect_true(all(c("week", "current", "avg_5yr", "min_5yr", "max_5yr", "product") %in% names(ss)))
})

test_that("stocks_deviation computes surplus/deficit", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("CL"), date_range = NULL, expiry_range = c(1, 12))
  result <- ea_calc_fundamentals(filters)
  sd <- result$stocks_deviation

  expect_s3_class(sd, "data.frame")
  expect_true(all(c("date", "product", "deviation", "deviation_pct") %in% names(sd)))
})
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `Rscript -e 'testthat::test_file("tests/testthat/test-calc-fundamentals.R")'`
Expected: FAIL — `ea_calc_fundamentals` not found.

- [ ] **Step 3: Write the implementation**

Create `R/calc_fundamentals.R`:

```r
# ---- Fundamentals Calculation Layer ----
# EIA inventory analytics from the canonical snapshot.

# Map dashboard markets to EIA product categories
ea_market_to_eia_product <- function(markets) {
  mapping <- c(
    CL = "crude", BRN = "crude",
    HO = "distillate", RB = "gasoline", NG = "ng"
  )
  unique(mapping[markets[markets %in% names(mapping)]])
}

ea_calc_fundamentals <- function(filters) {
  markets <- ea_coalesce(filters$commodities, c("CL", "BRN", "RB"))
  products <- ea_market_to_eia_product(markets)

  stocks <- ea_load_dataset("eia_stocks_long") |>
    dplyr::filter(.data$product %in% products)

  capacity <- tryCatch(
    ea_load_dataset("eia_storage_capacity_long") |>
      dplyr::filter(.data$product %in% products),
    error = function(e) tibble::tibble()
  )

  # --- stocks_timeseries ---
  stocks_timeseries <- stocks |>
    dplyr::select(.data$date, .data$product, .data$location, .data$value, .data$unit)

  # --- stocks_seasonal: current year vs 5-year range by week ---
  latest_year <- as.integer(format(max(stocks$date, na.rm = TRUE), "%Y"))
  five_year_start <- latest_year - 5L

  stocks_weekly <- stocks |>
    dplyr::mutate(
      year = as.integer(format(.data$date, "%Y")),
      week_num = as.integer(format(.data$date, "%V"))
    )

  historical <- stocks_weekly |>
    dplyr::filter(.data$year >= five_year_start, .data$year < latest_year) |>
    dplyr::group_by(.data$product, .data$location, .data$week_num) |>
    dplyr::summarise(
      avg_5yr = mean(.data$value, na.rm = TRUE),
      min_5yr = min(.data$value, na.rm = TRUE),
      max_5yr = max(.data$value, na.rm = TRUE),
      .groups = "drop"
    )

  current_year <- stocks_weekly |>
    dplyr::filter(.data$year == latest_year) |>
    dplyr::select(.data$product, .data$location, week_num = .data$week_num, current = .data$value)

  stocks_seasonal <- historical |>
    dplyr::left_join(current_year, by = c("product", "location", "week_num")) |>
    dplyr::rename(week = .data$week_num) |>
    dplyr::arrange(.data$product, .data$week)

  # --- stocks_deviation: surplus/deficit vs 5-year average ---
  stocks_deviation <- stocks_seasonal |>
    dplyr::filter(!is.na(.data$current)) |>
    dplyr::mutate(
      deviation = .data$current - .data$avg_5yr,
      deviation_pct = (.data$current - .data$avg_5yr) / .data$avg_5yr
    ) |>
    dplyr::left_join(
      current_year |> dplyr::select(.data$product, .data$location, .data$week_num) |>
        dplyr::left_join(
          stocks_weekly |>
            dplyr::filter(.data$year == latest_year) |>
            dplyr::select(.data$product, .data$location, .data$week_num, .data$date),
          by = c("product", "location", "week_num")
        ),
      by = c("product", "location", "week" = "week_num")
    ) |>
    dplyr::select(.data$date, .data$product, .data$deviation, .data$deviation_pct) |>
    dplyr::filter(!is.na(.data$date))

  # --- storage_capacity_util ---
  storage_capacity_util <- if (nrow(capacity) > 0) {
    latest_cap <- capacity |>
      dplyr::filter(.data$date == max(.data$date, na.rm = TRUE)) |>
      dplyr::select(.data$product, .data$location, capacity = .data$value)

    # Get latest stocks per product/location for utilization
    latest_stocks <- stocks |>
      dplyr::group_by(.data$product, .data$location) |>
      dplyr::filter(.data$date == max(.data$date, na.rm = TRUE)) |>
      dplyr::ungroup() |>
      dplyr::select(.data$date, .data$product, .data$location, stocks = .data$value)

    latest_stocks |>
      dplyr::inner_join(latest_cap, by = c("product", "location")) |>
      dplyr::mutate(utilization_pct = .data$stocks / .data$capacity) |>
      dplyr::select(.data$date, .data$product, .data$capacity, .data$stocks, .data$utilization_pct)
  } else {
    tibble::tibble(
      date = as.Date(character()), product = character(),
      capacity = numeric(), stocks = numeric(), utilization_pct = numeric()
    )
  }

  list(
    stocks_timeseries = stocks_timeseries,
    stocks_seasonal = stocks_seasonal,
    stocks_deviation = stocks_deviation,
    storage_capacity_util = storage_capacity_util
  )
}
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `Rscript -e 'testthat::test_file("tests/testthat/test-calc-fundamentals.R")'`
Expected: All PASS.

- [ ] **Step 5: Commit**

```bash
git add R/calc_fundamentals.R tests/testthat/test-calc-fundamentals.R
git commit -m "feat: add calc_fundamentals.R — EIA inventory analytics"
```

---

## Task 4: calc_volatility.R — Vol Surface & Realized Vol

**Files:**
- Create: `R/calc_volatility.R`
- Test: `tests/testthat/test-calc-volatility.R`

- [ ] **Step 1: Write the failing tests**

Create `tests/testthat/test-calc-volatility.R`:

```r
test_that("ea_calc_volatility returns all expected outputs", {
  skip_if_no_snapshot()
  filters <- list(
    commodities = c("CL", "BRN"),
    expiry_range = c(1, 12),
    date_range = NULL,
    rolling_window = "63D"
  )
  result <- ea_calc_volatility(filters)

  expect_type(result, "list")
  expect_true(all(c(
    "realized_vol_timeseries", "garch_vol", "vol_term_structure",
    "vol_surface_grid", "vol_skew_snapshot", "realized_vs_implied"
  ) %in% names(result)))
})

test_that("realized_vol_timeseries computes multiple windows", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("CL"), expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_volatility(filters)
  rv <- result$realized_vol_timeseries

  expect_s3_class(rv, "data.frame")
  expect_true(all(c("date", "market", "window", "realized_vol") %in% names(rv)))
  expect_true(all(c("20d", "60d", "120d") %in% unique(rv$window)))
})

test_that("garch_vol returns GARCH conditional vol", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("CL"), expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_volatility(filters)
  gv <- result$garch_vol

  expect_s3_class(gv, "data.frame")
  expect_true(all(c("date", "returns", "garch_vol") %in% names(gv)))
  expect_gt(nrow(gv), 100)
})

test_that("vol_term_structure shows vol by tenor", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("CL"), expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_volatility(filters)
  vts <- result$vol_term_structure

  expect_s3_class(vts, "data.frame")
  expect_true(all(c("market", "tenor", "realized_vol") %in% names(vts)))
})

test_that("vol_surface_grid has moneyness and tenor dimensions", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("CL"), expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_volatility(filters)
  vsg <- result$vol_surface_grid

  expect_s3_class(vsg, "data.frame")
  expect_true(all(c("market", "curve_point_num", "moneyness", "iv") %in% names(vsg)))
})
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `Rscript -e 'testthat::test_file("tests/testthat/test-calc-volatility.R")'`
Expected: FAIL — `ea_calc_volatility` not found.

- [ ] **Step 3: Write the implementation**

Create `R/calc_volatility.R`:

```r
# ---- Volatility Calculation Layer ----
# Realized vol, GARCH, vol surface, and implied vs realized analytics.

ea_calc_volatility <- function(filters) {
  catalog <- ea_market_catalog()
  curves <- ea_load_filtered_curves(filters)
  markets <- unique(curves$market)
  primary <- markets[1]
  labels <- stats::setNames(catalog$label, catalog$market)

  # Front contract returns for realized vol
  front <- curves |>
    dplyr::filter(.data$curve_point_num == 1) |>
    dplyr::arrange(.data$market, .data$date) |>
    dplyr::group_by(.data$market) |>
    dplyr::mutate(log_return = log(.data$value / dplyr::lag(.data$value))) |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(.data$log_return))

  # --- realized_vol_timeseries: rolling vol at 20d, 60d, 120d ---
  windows <- c("20d" = 20L, "60d" = 60L, "120d" = 120L)
  annualize <- sqrt(252)

  realized_vol_timeseries <- purrr::map_dfr(names(windows), function(w) {
    n <- windows[[w]]
    front |>
      dplyr::group_by(.data$market) |>
      dplyr::mutate(
        realized_vol = slider::slide_dbl(
          .data$log_return, sd, .before = n - 1L, .complete = TRUE
        ) * annualize
      ) |>
      dplyr::ungroup() |>
      dplyr::filter(!is.na(.data$realized_vol)) |>
      dplyr::transmute(
        date = .data$date, market = .data$market,
        window = w, realized_vol = .data$realized_vol
      )
  })

  # --- garch_vol: GARCH(1,1) for primary market ---
  primary_front <- front |>
    dplyr::filter(.data$market == primary) |>
    dplyr::select(.data$date, value = .data$log_return)

  garch_vol <- tryCatch({
    garch_xts <- RTL::garch(x = primary_front, out = "data")
    tibble::tibble(
      date = as.Date(zoo::index(garch_xts)),
      returns = as.numeric(zoo::coredata(garch_xts[, "returns"])),
      garch_vol = as.numeric(zoo::coredata(garch_xts[, "garch"]))
    )
  }, error = function(e) {
    tibble::tibble(date = as.Date(character()), returns = numeric(), garch_vol = numeric())
  })

  # --- vol_term_structure: realized vol by tenor ---
  vol_term_structure <- curves |>
    dplyr::arrange(.data$market, .data$curve_point_num, .data$date) |>
    dplyr::group_by(.data$market, .data$curve_point_num) |>
    dplyr::mutate(log_return = log(.data$value / dplyr::lag(.data$value))) |>
    dplyr::filter(!is.na(.data$log_return)) |>
    dplyr::summarise(
      realized_vol = sd(.data$log_return, na.rm = TRUE) * annualize,
      .groups = "drop"
    ) |>
    dplyr::rename(tenor = .data$curve_point_num)

  # --- vol_surface_grid: parametric IV surface ---
  latest_date <- max(curves$date, na.rm = TRUE)
  forwards_df <- curves |> dplyr::filter(.data$date == latest_date)

  vol_surface_grid <- ea_generate_vol_surface(markets, forwards_df, catalog)

  # --- vol_skew_snapshot: front tenor IV smile ---
  front_tenor <- min(vol_surface_grid$curve_point_num, na.rm = TRUE)
  vol_skew_snapshot <- vol_surface_grid |>
    dplyr::filter(.data$curve_point_num == front_tenor) |>
    dplyr::select(.data$market, .data$moneyness, .data$iv)

  # --- realized_vs_implied: 60d realized vs ATM implied ---
  rv_60 <- realized_vol_timeseries |>
    dplyr::filter(.data$window == "60d") |>
    dplyr::select(.data$date, .data$market, realized = .data$realized_vol)

  # ATM implied from surface (most recent snapshot only — static line)
  atm_iv <- vol_surface_grid |>
    dplyr::filter(abs(.data$moneyness - 1.0) < 0.01, .data$curve_point_num == front_tenor) |>
    dplyr::select(.data$market, implied = .data$iv)

  realized_vs_implied <- rv_60 |>
    dplyr::left_join(atm_iv, by = "market")

  list(
    realized_vol_timeseries = realized_vol_timeseries,
    garch_vol = garch_vol,
    vol_term_structure = vol_term_structure,
    vol_surface_grid = vol_surface_grid,
    vol_skew_snapshot = vol_skew_snapshot,
    realized_vs_implied = realized_vs_implied
  )
}
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `Rscript -e 'testthat::test_file("tests/testthat/test-calc-volatility.R")'`
Expected: All PASS.

- [ ] **Step 5: Commit**

```bash
git add R/calc_volatility.R tests/testthat/test-calc-volatility.R
git commit -m "feat: add calc_volatility.R — realized vol, GARCH, vol surface"
```

---

## Task 5: calc_codynamics.R — Cross-Market Relationships

**Files:**
- Create: `R/calc_codynamics.R`
- Test: `tests/testthat/test-calc-codynamics.R`

- [ ] **Step 1: Write the failing tests**

Create `tests/testthat/test-calc-codynamics.R`:

```r
test_that("ea_calc_codynamics returns all expected outputs", {
  skip_if_no_snapshot()
  filters <- list(
    commodities = c("CL", "BRN", "NG"),
    comparison_commodity = "HO",
    expiry_range = c(1, 12),
    date_range = NULL,
    rolling_window = "63D"
  )
  result <- ea_calc_codynamics(filters)

  expect_type(result, "list")
  expect_true(all(c(
    "correlation_matrix", "correlation_timeseries", "spread_timeseries",
    "spread_zscore", "beta_matrix", "pca_decomposition"
  ) %in% names(result)))
})

test_that("correlation_matrix is symmetric and bounded", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("CL", "BRN", "NG"), expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_codynamics(filters)
  cm <- result$correlation_matrix

  expect_s3_class(cm, "data.frame")
  expect_true(all(c("market_x", "market_y", "correlation") %in% names(cm)))
  expect_true(all(cm$correlation >= -1 & cm$correlation <= 1))
})

test_that("spread_zscore identifies stretched spreads", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("CL", "BRN"), expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_codynamics(filters)
  sz <- result$spread_zscore

  expect_s3_class(sz, "data.frame")
  expect_true(all(c("spread_label", "current", "mean", "std", "zscore") %in% names(sz)))
})

test_that("pca_decomposition has components for each market", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("CL", "BRN", "NG"), expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_codynamics(filters)
  pca <- result$pca_decomposition

  expect_s3_class(pca, "data.frame")
  expect_true("market" %in% names(pca))
  expect_true(all(c("PC1", "PC2", "PC3") %in% names(pca)))
})
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `Rscript -e 'testthat::test_file("tests/testthat/test-calc-codynamics.R")'`
Expected: FAIL — `ea_calc_codynamics` not found.

- [ ] **Step 3: Write the implementation**

Create `R/calc_codynamics.R`:

```r
# ---- Co-Dynamics Calculation Layer ----
# Cross-market correlations, spreads, betas, and PCA.

ea_parse_rolling_window <- function(window_str) {
  n <- as.integer(gsub("[^0-9]", "", window_str))
  if (is.na(n)) 63L else n
}

ea_calc_codynamics <- function(filters) {
  catalog <- ea_market_catalog()
  curves <- ea_load_filtered_curves(filters)
  markets <- unique(curves$market)
  comparison <- ea_coalesce(filters$comparison_commodity, "NG")
  focus_markets <- unique(c(markets, comparison))
  window <- ea_parse_rolling_window(ea_coalesce(filters$rolling_window, "63D"))

  # Front contract returns for all focus markets
  all_curves <- ea_load_dataset("commodity_curve_long") |>
    dplyr::filter(.data$market %in% focus_markets, .data$curve_point_num == 1) |>
    dplyr::arrange(.data$market, .data$date) |>
    dplyr::group_by(.data$market) |>
    dplyr::mutate(log_return = log(.data$value / dplyr::lag(.data$value))) |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(.data$log_return))

  # Wide returns matrix for correlation/PCA
  returns_wide <- all_curves |>
    dplyr::select(.data$date, .data$market, .data$log_return) |>
    tidyr::pivot_wider(names_from = .data$market, values_from = .data$log_return) |>
    dplyr::arrange(.data$date) |>
    stats::na.omit()

  market_cols <- setdiff(names(returns_wide), "date")
  returns_mat <- as.matrix(returns_wide[, market_cols])

  # --- correlation_matrix: latest rolling correlation ---
  if (nrow(returns_mat) >= window) {
    tail_mat <- utils::tail(returns_mat, window)
    corr_mat <- stats::cor(tail_mat, use = "pairwise.complete.obs")
  } else {
    corr_mat <- stats::cor(returns_mat, use = "pairwise.complete.obs")
  }

  correlation_matrix <- expand.grid(
    market_x = market_cols, market_y = market_cols,
    stringsAsFactors = FALSE
  ) |>
    tibble::as_tibble() |>
    dplyr::rowwise() |>
    dplyr::mutate(correlation = corr_mat[.data$market_x, .data$market_y]) |>
    dplyr::ungroup()

  # --- correlation_timeseries: rolling correlation between pairs ---
  pairs <- utils::combn(market_cols, 2, simplify = FALSE)

  correlation_timeseries <- purrr::map_dfr(pairs, function(pr) {
    x <- returns_wide[[pr[1]]]
    y <- returns_wide[[pr[2]]]

    roll_corr <- slider::slide2_dbl(
      x, y,
      ~ stats::cor(.x, .y, use = "pairwise.complete.obs"),
      .before = window - 1L, .complete = TRUE
    )

    tibble::tibble(
      date = returns_wide$date,
      pair = paste(pr[1], "vs", pr[2]),
      correlation = roll_corr,
      window = paste0(window, "d")
    ) |> dplyr::filter(!is.na(.data$correlation))
  })

  # --- spread_timeseries: intercommodity spreads ---
  front_prices <- all_curves |>
    dplyr::select(.data$date, .data$market, .data$value)

  spread_definitions <- list(
    "CL-BRN" = c("CL", "BRN"),
    "CL-RB (Crack)" = c("RB", "CL"),
    "CL-HO (Crack)" = c("HO", "CL")
  )

  spread_timeseries <- purrr::map_dfr(names(spread_definitions), function(sp_lbl) {
    legs <- spread_definitions[[sp_lbl]]
    if (!all(legs %in% focus_markets)) return(tibble::tibble())

    wide_sp <- front_prices |>
      dplyr::filter(.data$market %in% legs) |>
      tidyr::pivot_wider(names_from = .data$market, values_from = .data$value) |>
      dplyr::filter(!is.na(!!dplyr::sym(legs[1])), !is.na(!!dplyr::sym(legs[2])))

    if (nrow(wide_sp) == 0L) return(tibble::tibble())

    tibble::tibble(
      date = wide_sp$date,
      spread_label = sp_lbl,
      value = wide_sp[[legs[1]]] - wide_sp[[legs[2]]]
    )
  })

  # --- spread_zscore ---
  spread_zscore <- spread_timeseries |>
    dplyr::group_by(.data$spread_label) |>
    dplyr::summarise(
      current = dplyr::last(.data$value),
      mean = mean(.data$value, na.rm = TRUE),
      std = sd(.data$value, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(zscore = (.data$current - .data$mean) / .data$std)

  # --- beta_matrix: pairwise regression betas ---
  beta_matrix <- purrr::map_dfr(pairs, function(pr) {
    df <- returns_wide |>
      dplyr::select(.data$date, x = dplyr::all_of(pr[1]), y = dplyr::all_of(pr[2])) |>
      stats::na.omit()

    if (nrow(df) < 30) {
      return(tibble::tibble(
        market_x = pr[1], market_y = pr[2], beta = NA_real_, r_squared = NA_real_
      ))
    }

    fit <- stats::lm(y ~ x, data = df)
    tidy_fit <- broom::glance(fit)

    tibble::tibble(
      market_x = pr[1], market_y = pr[2],
      beta = stats::coef(fit)[["x"]],
      r_squared = tidy_fit$r.squared
    )
  })

  # --- pca_decomposition ---
  pca_decomposition <- tryCatch({
    pca <- stats::prcomp(returns_mat, center = TRUE, scale. = TRUE)
    loadings <- as.data.frame(pca$rotation[, 1:min(3, ncol(pca$rotation))])
    n_pcs <- ncol(loadings)
    colnames(loadings) <- paste0("PC", seq_len(n_pcs))
    loadings$market <- rownames(loadings)

    # Pad missing PCs if fewer than 3 markets
    for (pc in c("PC1", "PC2", "PC3")) {
      if (!pc %in% names(loadings)) loadings[[pc]] <- NA_real_
    }

    tibble::as_tibble(loadings) |>
      dplyr::select(.data$market, .data$PC1, .data$PC2, .data$PC3)
  }, error = function(e) {
    tibble::tibble(market = market_cols, PC1 = NA_real_, PC2 = NA_real_, PC3 = NA_real_)
  })

  list(
    correlation_matrix = correlation_matrix,
    correlation_timeseries = correlation_timeseries,
    spread_timeseries = spread_timeseries,
    spread_zscore = spread_zscore,
    beta_matrix = beta_matrix,
    pca_decomposition = pca_decomposition
  )
}
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `Rscript -e 'testthat::test_file("tests/testthat/test-calc-codynamics.R")'`
Expected: All PASS.

- [ ] **Step 5: Commit**

```bash
git add R/calc_codynamics.R tests/testthat/test-calc-codynamics.R
git commit -m "feat: add calc_codynamics.R — correlations, spreads, betas, PCA"
```

---

## Task 6: calc_seasonality.R — Seasonal Patterns

**Files:**
- Create: `R/calc_seasonality.R`
- Test: `tests/testthat/test-calc-seasonality.R`

- [ ] **Step 1: Write the failing tests**

Create `tests/testthat/test-calc-seasonality.R`:

```r
test_that("ea_calc_seasonality returns all expected outputs", {
  skip_if_no_snapshot()
  filters <- list(
    commodities = c("CL", "NG"),
    expiry_range = c(1, 12),
    date_range = NULL
  )
  result <- ea_calc_seasonality(filters)

  expect_type(result, "list")
  expect_true(all(c(
    "seasonal_overlay", "seasonal_range", "seasonal_returns",
    "mean_reversion", "seasonal_deviation"
  ) %in% names(result)))
})

test_that("seasonal_overlay includes multiple years", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("CL"), expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_seasonality(filters)
  so <- result$seasonal_overlay

  expect_s3_class(so, "data.frame")
  expect_true(all(c("day_of_year", "year", "market", "indexed_value") %in% names(so)))
  expect_gt(length(unique(so$year)), 1)
})

test_that("seasonal_range has 5-year bands", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("CL"), expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_seasonality(filters)
  sr <- result$seasonal_range

  expect_s3_class(sr, "data.frame")
  expect_true(all(c("day_of_year", "market", "avg", "min", "max") %in% names(sr)))
})

test_that("mean_reversion returns OU parameters", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("CL"), expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_seasonality(filters)
  mr <- result$mean_reversion

  expect_s3_class(mr, "data.frame")
  expect_true(all(c("market", "theta", "mu", "sigma", "half_life") %in% names(mr)))
})

test_that("seasonal_returns has hit rates by month", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("CL"), expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_seasonality(filters)
  sret <- result$seasonal_returns

  expect_s3_class(sret, "data.frame")
  expect_true(all(c("market", "period", "avg_return", "hit_rate") %in% names(sret)))
})
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `Rscript -e 'testthat::test_file("tests/testthat/test-calc-seasonality.R")'`
Expected: FAIL — `ea_calc_seasonality` not found.

- [ ] **Step 3: Write the implementation**

Create `R/calc_seasonality.R`:

```r
# ---- Seasonality Calculation Layer ----
# Seasonal patterns, historical overlays, and mean reversion.

ea_calc_seasonality <- function(filters) {
  catalog <- ea_market_catalog()
  curves <- ea_load_filtered_curves(filters)
  markets <- unique(curves$market)

  # Front contract prices
  front <- curves |>
    dplyr::filter(.data$curve_point_num == 1) |>
    dplyr::select(.data$date, .data$market, .data$value)

  latest_year <- as.integer(format(max(front$date, na.rm = TRUE), "%Y"))

  # --- seasonal_overlay: current year vs historical years, indexed to 100 ---
  seasonal_overlay <- front |>
    dplyr::mutate(
      year = as.integer(format(.data$date, "%Y")),
      day_of_year = as.integer(format(.data$date, "%j"))
    ) |>
    dplyr::group_by(.data$market, .data$year) |>
    dplyr::mutate(indexed_value = .data$value / dplyr::first(.data$value) * 100) |>
    dplyr::ungroup() |>
    dplyr::filter(.data$year >= latest_year - 5L) |>
    dplyr::select(.data$day_of_year, .data$year, .data$market, .data$indexed_value)

  # --- seasonal_range: 5-year min/max/mean band ---
  historical_indexed <- seasonal_overlay |>
    dplyr::filter(.data$year < latest_year, .data$year >= latest_year - 5L)

  seasonal_range <- historical_indexed |>
    dplyr::group_by(.data$market, .data$day_of_year) |>
    dplyr::summarise(
      avg = mean(.data$indexed_value, na.rm = TRUE),
      min = min(.data$indexed_value, na.rm = TRUE),
      max = max(.data$indexed_value, na.rm = TRUE),
      .groups = "drop"
    )

  # --- seasonal_returns: avg monthly return and hit rate ---
  monthly_returns <- front |>
    dplyr::arrange(.data$market, .data$date) |>
    dplyr::group_by(.data$market) |>
    dplyr::mutate(
      log_return = log(.data$value / dplyr::lag(.data$value)),
      period = format(.data$date, "%b")
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(.data$log_return))

  seasonal_returns <- monthly_returns |>
    dplyr::group_by(.data$market, .data$period) |>
    dplyr::summarise(
      avg_return = mean(.data$log_return, na.rm = TRUE),
      hit_rate = mean(.data$log_return > 0, na.rm = TRUE),
      .groups = "drop"
    )

  # --- mean_reversion: OU fit on M1-M2 spread ---
  mean_reversion <- purrr::map_dfr(markets, function(mkt) {
    mkt_curves <- curves |>
      dplyr::filter(.data$market == mkt, .data$curve_point_num %in% c(1, 2))

    wide <- mkt_curves |>
      dplyr::select(.data$date, .data$curve_point, .data$value) |>
      tidyr::pivot_wider(names_from = .data$curve_point, values_from = .data$value) |>
      dplyr::arrange(.data$date) |>
      stats::na.omit()

    if (nrow(wide) < 30 || !"01" %in% names(wide) || !"02" %in% names(wide)) {
      return(tibble::tibble(
        market = mkt, theta = NA_real_, mu = NA_real_,
        sigma = NA_real_, half_life = NA_real_
      ))
    }

    spread <- wide[["01"]] - wide[["02"]]

    tryCatch({
      ou <- RTL::fitOU(spread = spread)
      tibble::tibble(
        market = mkt, theta = ou$theta, mu = ou$mu,
        sigma = ou$sigma, half_life = ou$halfLife
      )
    }, error = function(e) {
      tibble::tibble(
        market = mkt, theta = NA_real_, mu = NA_real_,
        sigma = NA_real_, half_life = NA_real_
      )
    })
  })

  # --- seasonal_deviation: current year percentile in historical distribution ---
  current_indexed <- seasonal_overlay |>
    dplyr::filter(.data$year == latest_year) |>
    dplyr::select(.data$day_of_year, .data$market, current_val = .data$indexed_value)

  seasonal_deviation <- current_indexed |>
    dplyr::left_join(
      historical_indexed |>
        dplyr::group_by(.data$market, .data$day_of_year) |>
        dplyr::summarise(
          values = list(.data$indexed_value),
          .groups = "drop"
        ),
      by = c("market", "day_of_year")
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      percentile = if (length(.data$values) > 0) {
        mean(.data$values <= .data$current_val)
      } else {
        NA_real_
      }
    ) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      date = as.Date(paste0(latest_year, "-01-01")) + .data$day_of_year - 1L,
      market = .data$market,
      percentile = .data$percentile
    )

  list(
    seasonal_overlay = seasonal_overlay,
    seasonal_range = seasonal_range,
    seasonal_returns = seasonal_returns,
    mean_reversion = mean_reversion,
    seasonal_deviation = seasonal_deviation
  )
}
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `Rscript -e 'testthat::test_file("tests/testthat/test-calc-seasonality.R")'`
Expected: All PASS.

- [ ] **Step 5: Commit**

```bash
git add R/calc_seasonality.R tests/testthat/test-calc-seasonality.R
git commit -m "feat: add calc_seasonality.R — seasonal patterns and mean reversion"
```

---

## Task 7: calc_hedging.R — Hedge Ratios & Effectiveness

**Files:**
- Create: `R/calc_hedging.R`
- Test: `tests/testthat/test-calc-hedging.R`

- [ ] **Step 1: Write the failing tests**

Create `tests/testthat/test-calc-hedging.R`:

```r
test_that("ea_calc_hedging returns all expected outputs", {
  skip_if_no_snapshot()
  filters <- list(
    commodities = c("CL", "BRN", "HO"),
    comparison_commodity = "CL",
    expiry_range = c(1, 12),
    date_range = NULL,
    rolling_window = "63D"
  )
  result <- ea_calc_hedging(filters)

  expect_type(result, "list")
  expect_true(all(c(
    "hedge_ratios", "rolling_beta", "hedge_effectiveness",
    "residual_timeseries", "ou_fit", "cross_hedge_matrix"
  ) %in% names(result)))
})

test_that("hedge_ratios returns OLS betas", {
  skip_if_no_snapshot()
  filters <- list(
    commodities = c("BRN", "HO"),
    comparison_commodity = "CL",
    expiry_range = c(1, 12),
    date_range = NULL
  )
  result <- ea_calc_hedging(filters)
  hr <- result$hedge_ratios

  expect_s3_class(hr, "data.frame")
  expect_true(all(c("market", "beta", "r_squared", "std_error") %in% names(hr)))
  expect_gt(nrow(hr), 0)
})

test_that("rolling_beta has time dimension", {
  skip_if_no_snapshot()
  filters <- list(
    commodities = c("BRN"),
    comparison_commodity = "CL",
    expiry_range = c(1, 12),
    date_range = NULL,
    rolling_window = "63D"
  )
  result <- ea_calc_hedging(filters)
  rb <- result$rolling_beta

  expect_s3_class(rb, "data.frame")
  expect_true(all(c("date", "market", "beta", "r_squared") %in% names(rb)))
  expect_gt(nrow(rb), 0)
})

test_that("hedge_effectiveness computes vol reduction", {
  skip_if_no_snapshot()
  filters <- list(
    commodities = c("BRN"),
    comparison_commodity = "CL",
    expiry_range = c(1, 12),
    date_range = NULL
  )
  result <- ea_calc_hedging(filters)
  he <- result$hedge_effectiveness

  expect_s3_class(he, "data.frame")
  expect_true(all(c("market", "unhedged_vol", "hedged_vol", "vol_reduction_pct") %in% names(he)))
})

test_that("ou_fit returns OU parameters on residuals", {
  skip_if_no_snapshot()
  filters <- list(
    commodities = c("BRN"),
    comparison_commodity = "CL",
    expiry_range = c(1, 12),
    date_range = NULL
  )
  result <- ea_calc_hedging(filters)
  ou <- result$ou_fit

  expect_s3_class(ou, "data.frame")
  expect_true(all(c("market", "theta", "mu", "sigma", "half_life") %in% names(ou)))
})
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `Rscript -e 'testthat::test_file("tests/testthat/test-calc-hedging.R")'`
Expected: FAIL — `ea_calc_hedging` not found.

- [ ] **Step 3: Write the implementation**

Create `R/calc_hedging.R`:

```r
# ---- Hedging Calculation Layer ----
# Hedge ratios, rolling betas, effectiveness, and mean reversion.

ea_calc_hedging <- function(filters) {
  catalog <- ea_market_catalog()
  markets <- ea_coalesce(filters$commodities, c("CL", "BRN", "RB"))
  benchmark <- ea_coalesce(filters$comparison_commodity, "CL")
  window <- ea_parse_rolling_window(ea_coalesce(filters$rolling_window, "63D"))
  annualize <- sqrt(252)

  # Hedge targets: markets that are NOT the benchmark
  targets <- setdiff(markets, benchmark)
  all_mkts <- unique(c(targets, benchmark))

  # Front contract log returns
  all_returns <- ea_load_dataset("commodity_curve_long") |>
    dplyr::filter(.data$market %in% all_mkts, .data$curve_point_num == 1) |>
    dplyr::arrange(.data$market, .data$date) |>
    dplyr::group_by(.data$market) |>
    dplyr::mutate(log_return = log(.data$value / dplyr::lag(.data$value))) |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(.data$log_return))

  returns_wide <- all_returns |>
    dplyr::select(.data$date, .data$market, .data$log_return) |>
    tidyr::pivot_wider(names_from = .data$market, values_from = .data$log_return) |>
    dplyr::arrange(.data$date) |>
    stats::na.omit()

  # --- hedge_ratios: OLS beta of each target vs benchmark ---
  hedge_ratios <- purrr::map_dfr(targets, function(mkt) {
    df <- returns_wide |> dplyr::select(y = dplyr::all_of(mkt), x = dplyr::all_of(benchmark))
    fit <- stats::lm(y ~ x, data = df)
    tidy_coef <- broom::tidy(fit)
    glance_fit <- broom::glance(fit)

    x_row <- tidy_coef[tidy_coef$term == "x", ]

    tibble::tibble(
      market = mkt,
      beta = x_row$estimate,
      r_squared = glance_fit$r.squared,
      std_error = x_row$std.error
    )
  })

  # --- rolling_beta: rolling OLS beta over time ---
  rolling_beta <- purrr::map_dfr(targets, function(mkt) {
    y_col <- returns_wide[[mkt]]
    x_col <- returns_wide[[benchmark]]

    roll_beta <- slider::slide2_dbl(
      y_col, x_col,
      ~ {
        if (length(.x) < 10) return(NA_real_)
        fit <- stats::lm(.x ~ .y)
        stats::coef(fit)[[2]]
      },
      .before = window - 1L, .complete = TRUE
    )

    roll_r2 <- slider::slide2_dbl(
      y_col, x_col,
      ~ {
        if (length(.x) < 10) return(NA_real_)
        fit <- stats::lm(.x ~ .y)
        summary(fit)$r.squared
      },
      .before = window - 1L, .complete = TRUE
    )

    tibble::tibble(
      date = returns_wide$date,
      market = mkt,
      beta = roll_beta,
      r_squared = roll_r2
    ) |> dplyr::filter(!is.na(.data$beta))
  })

  # --- hedge_effectiveness: hedged vs unhedged vol ---
  hedge_effectiveness <- purrr::map_dfr(targets, function(mkt) {
    df <- returns_wide |> dplyr::select(y = dplyr::all_of(mkt), x = dplyr::all_of(benchmark))
    fit <- stats::lm(y ~ x, data = df)
    resid <- stats::residuals(fit)

    unhedged <- sd(df$y, na.rm = TRUE) * annualize
    hedged <- sd(resid, na.rm = TRUE) * annualize

    tibble::tibble(
      market = mkt,
      unhedged_vol = unhedged,
      hedged_vol = hedged,
      vol_reduction_pct = 1 - hedged / unhedged
    )
  })

  # --- residual_timeseries ---
  residual_timeseries <- purrr::map_dfr(targets, function(mkt) {
    df <- returns_wide |> dplyr::select(date = .data$date, y = dplyr::all_of(mkt), x = dplyr::all_of(benchmark))
    fit <- stats::lm(y ~ x, data = df)

    tibble::tibble(
      date = df$date,
      market = mkt,
      residual = as.numeric(stats::residuals(fit))
    )
  })

  # --- ou_fit: OU parameters on hedge residuals ---
  ou_fit <- purrr::map_dfr(targets, function(mkt) {
    resid <- residual_timeseries |>
      dplyr::filter(.data$market == mkt) |>
      dplyr::pull(.data$residual)

    if (length(resid) < 30) {
      return(tibble::tibble(
        market = mkt, theta = NA_real_, mu = NA_real_,
        sigma = NA_real_, half_life = NA_real_
      ))
    }

    # cumulative residual for OU fitting (levels, not returns)
    cum_resid <- cumsum(resid)

    tryCatch({
      ou <- RTL::fitOU(spread = cum_resid)
      tibble::tibble(
        market = mkt, theta = ou$theta, mu = ou$mu,
        sigma = ou$sigma, half_life = ou$halfLife
      )
    }, error = function(e) {
      tibble::tibble(
        market = mkt, theta = NA_real_, mu = NA_real_,
        sigma = NA_real_, half_life = NA_real_
      )
    })
  })

  # --- cross_hedge_matrix: pairwise beta/R-squared ---
  all_pairs <- utils::combn(all_mkts, 2, simplify = FALSE)
  cross_hedge_matrix <- purrr::map_dfr(all_pairs, function(pr) {
    if (!all(pr %in% names(returns_wide))) {
      return(tibble::tibble(
        market_x = pr[1], market_y = pr[2], beta = NA_real_, r_squared = NA_real_
      ))
    }
    df <- returns_wide |> dplyr::select(x = dplyr::all_of(pr[1]), y = dplyr::all_of(pr[2]))
    fit <- stats::lm(y ~ x, data = df)
    tibble::tibble(
      market_x = pr[1], market_y = pr[2],
      beta = stats::coef(fit)[["x"]],
      r_squared = broom::glance(fit)$r.squared
    )
  })

  list(
    hedge_ratios = hedge_ratios,
    rolling_beta = rolling_beta,
    hedge_effectiveness = hedge_effectiveness,
    residual_timeseries = residual_timeseries,
    ou_fit = ou_fit,
    cross_hedge_matrix = cross_hedge_matrix
  )
}
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `Rscript -e 'testthat::test_file("tests/testthat/test-calc-hedging.R")'`
Expected: All PASS.

- [ ] **Step 5: Commit**

```bash
git add R/calc_hedging.R tests/testthat/test-calc-hedging.R
git commit -m "feat: add calc_hedging.R — hedge ratios, effectiveness, mean reversion"
```

---

## Task 8: calc_overview.R — Morning Briefing

**Files:**
- Create: `R/calc_overview.R`
- Test: `tests/testthat/test-calc-overview.R`

- [ ] **Step 1: Write the failing tests**

Create `tests/testthat/test-calc-overview.R`:

```r
test_that("ea_calc_overview returns all expected outputs", {
  skip_if_no_snapshot()
  filters <- list(
    commodities = c("CL", "BRN", "NG"),
    comparison_commodity = "HO",
    expiry_range = c(1, 12),
    date_range = NULL,
    rolling_window = "63D"
  )
  result <- ea_calc_overview(filters)

  expect_type(result, "list")
  expect_true(all(c(
    "price_snapshot", "relative_performance", "curve_structure_summary",
    "correlation_snapshot", "spread_monitor", "vol_snapshot", "drawdown_summary"
  ) %in% names(result)))
})

test_that("price_snapshot has current prices and changes", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("CL", "BRN"), expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_overview(filters)
  ps <- result$price_snapshot

  expect_s3_class(ps, "data.frame")
  expect_true(all(c("market", "price", "change", "change_pct") %in% names(ps)))
  expect_true(all(ps$market %in% c("CL", "BRN")))
})

test_that("relative_performance is indexed to 100", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("CL", "BRN"), expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_overview(filters)
  rp <- result$relative_performance

  expect_s3_class(rp, "data.frame")
  expect_true(all(c("date", "market", "indexed_value") %in% names(rp)))
})

test_that("drawdown_summary computes drawdowns", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("CL"), expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_overview(filters)
  dd <- result$drawdown_summary

  expect_s3_class(dd, "data.frame")
  expect_true(all(c("market", "drawdown_pct", "days_from_peak") %in% names(dd)))
})

test_that("vol_snapshot includes percentile rank", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("CL"), expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_overview(filters)
  vs <- result$vol_snapshot

  expect_s3_class(vs, "data.frame")
  expect_true(all(c("market", "realized_vol_20d", "vol_percentile") %in% names(vs)))
})
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `Rscript -e 'testthat::test_file("tests/testthat/test-calc-overview.R")'`
Expected: FAIL — `ea_calc_overview` not found.

- [ ] **Step 3: Write the implementation**

Create `R/calc_overview.R`:

```r
# ---- Overview Calculation Layer ----
# Morning briefing: price snapshot, relative performance, curve shape, vol, drawdowns.

ea_calc_overview <- function(filters) {
  catalog <- ea_market_catalog()
  curves <- ea_load_filtered_curves(filters)
  markets <- unique(curves$market)
  labels <- stats::setNames(catalog$label, catalog$market)
  latest_date <- max(curves$date, na.rm = TRUE)
  annualize <- sqrt(252)

  # Front contract series
  front <- curves |>
    dplyr::filter(.data$curve_point_num == 1) |>
    dplyr::arrange(.data$market, .data$date)

  # --- price_snapshot ---
  price_snapshot <- front |>
    dplyr::group_by(.data$market) |>
    dplyr::filter(dplyr::n() >= 2) |>
    dplyr::summarise(
      price = dplyr::last(.data$value),
      change = dplyr::last(.data$value) - dplyr::nth(.data$value, dplyr::n() - 1L),
      change_pct = (.data$change / dplyr::nth(.data$value, dplyr::n() - 1L)),
      .groups = "drop"
    )

  # --- relative_performance: indexed to 100 over lookback ---
  lookback_start <- latest_date - 365L
  relative_performance <- front |>
    dplyr::filter(.data$date >= lookback_start) |>
    dplyr::group_by(.data$market) |>
    dplyr::mutate(indexed_value = .data$value / dplyr::first(.data$value) * 100) |>
    dplyr::ungroup() |>
    dplyr::select(.data$date, .data$market, .data$indexed_value)

  # --- curve_structure_summary ---
  latest_curves <- curves |> dplyr::filter(.data$date == latest_date)

  curve_structure_summary <- latest_curves |>
    dplyr::group_by(.data$market) |>
    dplyr::summarise(
      m1 = .data$value[.data$curve_point_num == min(.data$curve_point_num)][1],
      m2 = .data$value[.data$curve_point_num == (min(.data$curve_point_num) + 1L)][1],
      back = .data$value[.data$curve_point_num == max(.data$curve_point_num)][1],
      .groups = "drop"
    ) |>
    dplyr::mutate(
      m1_m2_spread = .data$m1 - .data$m2,
      slope = .data$back - .data$m1,
      structure_label = dplyr::case_when(
        .data$slope > 0.01 ~ "Contango",
        .data$slope < -0.01 ~ "Backwardation",
        TRUE ~ "Flat"
      )
    ) |>
    dplyr::select(.data$market, .data$m1_m2_spread, .data$slope, .data$structure_label)

  # --- correlation_snapshot ---
  returns_wide <- front |>
    dplyr::group_by(.data$market) |>
    dplyr::mutate(log_return = log(.data$value / dplyr::lag(.data$value))) |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(.data$log_return)) |>
    dplyr::select(.data$date, .data$market, .data$log_return) |>
    tidyr::pivot_wider(names_from = .data$market, values_from = .data$log_return) |>
    dplyr::arrange(.data$date) |>
    stats::na.omit()

  market_cols <- setdiff(names(returns_wide), "date")
  tail_mat <- utils::tail(as.matrix(returns_wide[, market_cols]), 63)
  corr_mat <- stats::cor(tail_mat, use = "pairwise.complete.obs")

  correlation_snapshot <- expand.grid(
    market_x = market_cols, market_y = market_cols, stringsAsFactors = FALSE
  ) |>
    tibble::as_tibble() |>
    dplyr::rowwise() |>
    dplyr::mutate(correlation = corr_mat[.data$market_x, .data$market_y]) |>
    dplyr::ungroup()

  # --- spread_monitor ---
  spread_defs <- list(
    "CL-BRN" = c("CL", "BRN"),
    "RB-CL (Crack)" = c("RB", "CL"),
    "HO-CL (Crack)" = c("HO", "CL")
  )

  front_wide <- front |>
    dplyr::select(.data$date, .data$market, .data$value) |>
    tidyr::pivot_wider(names_from = .data$market, values_from = .data$value) |>
    dplyr::arrange(.data$date)

  spread_monitor <- purrr::map_dfr(names(spread_defs), function(sp_lbl) {
    legs <- spread_defs[[sp_lbl]]
    if (!all(legs %in% names(front_wide))) return(tibble::tibble())

    spread_ts <- front_wide[[legs[1]]] - front_wide[[legs[2]]]
    valid <- !is.na(spread_ts)
    spread_ts <- spread_ts[valid]
    if (length(spread_ts) < 2) return(tibble::tibble())

    current <- spread_ts[length(spread_ts)]
    prev <- spread_ts[length(spread_ts) - 1L]
    rolling_mean <- mean(utils::tail(spread_ts, 63), na.rm = TRUE)
    rolling_sd <- sd(utils::tail(spread_ts, 63), na.rm = TRUE)

    tibble::tibble(
      spread_label = sp_lbl,
      level = current,
      change = current - prev,
      zscore = if (rolling_sd > 0) (current - rolling_mean) / rolling_sd else 0
    )
  })

  # --- vol_snapshot ---
  vol_snapshot <- front |>
    dplyr::group_by(.data$market) |>
    dplyr::mutate(log_return = log(.data$value / dplyr::lag(.data$value))) |>
    dplyr::filter(!is.na(.data$log_return)) |>
    dplyr::summarise(
      realized_vol_20d = sd(utils::tail(.data$log_return, 20)) * annualize,
      all_vols = list(
        slider::slide_dbl(.data$log_return, sd, .before = 19L, .complete = TRUE) * annualize
      ),
      .groups = "drop"
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      vol_percentile = {
        v <- unlist(.data$all_vols)
        v <- v[!is.na(v)]
        if (length(v) > 0) mean(v <= .data$realized_vol_20d) else NA_real_
      }
    ) |>
    dplyr::ungroup() |>
    dplyr::select(.data$market, .data$realized_vol_20d, .data$vol_percentile)

  # --- drawdown_summary ---
  drawdown_summary <- front |>
    dplyr::group_by(.data$market) |>
    dplyr::summarise(
      peak = max(.data$value, na.rm = TRUE),
      peak_date = .data$date[which.max(.data$value)],
      current = dplyr::last(.data$value),
      latest = dplyr::last(.data$date),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      drawdown_pct = (.data$current - .data$peak) / .data$peak,
      days_from_peak = as.integer(.data$latest - .data$peak_date)
    ) |>
    dplyr::select(.data$market, .data$drawdown_pct, .data$days_from_peak)

  list(
    price_snapshot = price_snapshot,
    relative_performance = relative_performance,
    curve_structure_summary = curve_structure_summary,
    correlation_snapshot = correlation_snapshot,
    spread_monitor = spread_monitor,
    vol_snapshot = vol_snapshot,
    drawdown_summary = drawdown_summary
  )
}
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `Rscript -e 'testthat::test_file("tests/testthat/test-calc-overview.R")'`
Expected: All PASS.

- [ ] **Step 5: Commit**

```bash
git add R/calc_overview.R tests/testthat/test-calc-overview.R
git commit -m "feat: add calc_overview.R — morning briefing analytics"
```

---

## Task 9: calc_scenarios.R — Stress Testing & Simulation

**Files:**
- Create: `R/calc_scenarios.R`
- Test: `tests/testthat/test-calc-scenarios.R`

- [ ] **Step 1: Write the failing tests**

Create `tests/testthat/test-calc-scenarios.R`:

```r
test_that("ea_calc_scenarios returns all expected outputs", {
  skip_if_no_snapshot()
  filters <- list(
    commodities = c("CL", "BRN"),
    comparison_commodity = "NG",
    expiry_range = c(1, 12),
    date_range = NULL
  )
  shocks <- list(flat = 0, vol = 0, spread = 0)
  result <- ea_calc_scenarios(filters, shocks)

  expect_type(result, "list")
  expect_true(all(c(
    "price_simulations", "ou_simulations", "stress_scenarios",
    "spread_option_pnl", "var_summary", "correlation_stress"
  ) %in% names(result)))
})

test_that("price_simulations returns GBM paths", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("CL"), expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_scenarios(filters, list(flat = 0, vol = 0, spread = 0))
  ps <- result$price_simulations

  expect_s3_class(ps, "data.frame")
  expect_true(all(c("t", "market", "sim_id", "price") %in% names(ps)))
  expect_gt(nrow(ps), 0)
})

test_that("var_summary computes VaR and CVaR", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("CL"), expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_scenarios(filters, list(flat = 0, vol = 0, spread = 0))
  vs <- result$var_summary

  expect_s3_class(vs, "data.frame")
  expect_true(all(c("market", "var_95", "var_99", "cvar_95", "cvar_99") %in% names(vs)))
  # VaR should be negative (loss)
  expect_true(all(vs$var_95 <= 0))
})

test_that("stress_scenarios applies predefined shocks", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("CL", "BRN"), expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_scenarios(filters, list(flat = 5, vol = 10, spread = 2))
  ss <- result$stress_scenarios

  expect_s3_class(ss, "data.frame")
  expect_true(all(c("scenario_label", "market", "shock_pct", "new_price", "pnl_impact") %in% names(ss)))
})

test_that("correlation_stress compares normal vs stress regimes", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("CL", "BRN", "NG"), expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_scenarios(filters, list(flat = 0, vol = 0, spread = 0))
  cs <- result$correlation_stress

  expect_s3_class(cs, "data.frame")
  expect_true(all(c("market_x", "market_y", "corr_normal", "corr_stress") %in% names(cs)))
})
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `Rscript -e 'testthat::test_file("tests/testthat/test-calc-scenarios.R")'`
Expected: FAIL — `ea_calc_scenarios` not found.

- [ ] **Step 3: Write the implementation**

Create `R/calc_scenarios.R`:

```r
# ---- Scenarios Calculation Layer ----
# Monte Carlo simulation, stress testing, VaR, and correlation stress.

ea_calc_scenarios <- function(filters, shocks = list(flat = 0, vol = 0, spread = 0)) {
  catalog <- ea_market_catalog()
  curves <- ea_load_filtered_curves(filters)
  markets <- unique(curves$market)
  annualize <- sqrt(252)

  # Front contract data
  front <- curves |>
    dplyr::filter(.data$curve_point_num == 1) |>
    dplyr::arrange(.data$market, .data$date)

  latest_prices <- front |>
    dplyr::group_by(.data$market) |>
    dplyr::summarise(price = dplyr::last(.data$value), .groups = "drop")

  # Log returns for calibration
  front_returns <- front |>
    dplyr::group_by(.data$market) |>
    dplyr::mutate(log_return = log(.data$value / dplyr::lag(.data$value))) |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(.data$log_return))

  # --- price_simulations: GBM Monte Carlo ---
  nsims <- 100L
  horizon <- 63L  # ~3 months

  price_simulations <- purrr::map_dfr(markets, function(mkt) {
    mkt_returns <- front_returns |> dplyr::filter(.data$market == mkt)
    S0 <- latest_prices$price[latest_prices$market == mkt]
    mu <- mean(mkt_returns$log_return, na.rm = TRUE) * 252
    sigma <- sd(mkt_returns$log_return, na.rm = TRUE) * annualize

    tryCatch({
      sim <- RTL::simGBM(
        nsims = nsims, S0 = S0, drift = mu, sigma = sigma,
        T2M = horizon / 252, dt = 1 / 252
      )

      sim_long <- sim |>
        tidyr::pivot_longer(
          cols = -"t", names_to = "sim_id", values_to = "price"
        ) |>
        dplyr::mutate(market = mkt)

      sim_long
    }, error = function(e) {
      tibble::tibble(t = numeric(), market = character(), sim_id = character(), price = numeric())
    })
  })

  # --- ou_simulations: mean-reverting spread sims ---
  spread_pairs <- list("CL-BRN" = c("CL", "BRN"))
  available_pairs <- purrr::keep(spread_pairs, ~ all(.x %in% markets))

  ou_simulations <- purrr::map_dfr(names(available_pairs), function(sp_lbl) {
    legs <- available_pairs[[sp_lbl]]
    spread_ts <- front |>
      dplyr::filter(.data$market %in% legs) |>
      dplyr::select(.data$date, .data$market, .data$value) |>
      tidyr::pivot_wider(names_from = .data$market, values_from = .data$value) |>
      stats::na.omit()

    if (nrow(spread_ts) < 30) return(tibble::tibble())

    spread_vals <- spread_ts[[legs[1]]] - spread_ts[[legs[2]]]
    S0 <- spread_vals[length(spread_vals)]

    tryCatch({
      ou_params <- RTL::fitOU(spread = spread_vals)
      sim <- RTL::simOU(
        nsims = nsims, S0 = S0,
        mu = ou_params$mu, theta = ou_params$theta, sigma = ou_params$sigma,
        T2M = horizon / 252, dt = 1 / 252
      )

      sim |>
        tidyr::pivot_longer(cols = -"t", names_to = "sim_id", values_to = "value") |>
        dplyr::mutate(spread_label = sp_lbl)
    }, error = function(e) {
      tibble::tibble(t = numeric(), spread_label = character(), sim_id = character(), value = numeric())
    })
  })

  # --- stress_scenarios ---
  scenarios <- tibble::tribble(
    ~scenario_label, ~shock_pct,
    "Supply squeeze (+15%)", 0.15,
    "Demand collapse (-20%)", -0.20,
    "Vol spike (flat price)", 0.0,
    "Custom shock", ea_coalesce(shocks$flat, 0) / 100
  )

  stress_scenarios <- purrr::map_dfr(seq_len(nrow(scenarios)), function(i) {
    sc <- scenarios[i, ]
    latest_prices |>
      dplyr::mutate(
        scenario_label = sc$scenario_label,
        shock_pct = sc$shock_pct,
        new_price = .data$price * (1 + sc$shock_pct),
        pnl_impact = .data$new_price - .data$price
      )
  })

  # --- spread_option_pnl ---
  spread_option_pnl <- tryCatch({
    if (length(markets) < 2) return(tibble::tibble())

    m1 <- markets[1]
    m2 <- markets[2]
    F1 <- latest_prices$price[latest_prices$market == m1]
    F2 <- latest_prices$price[latest_prices$market == m2]
    sig1 <- sd(front_returns$log_return[front_returns$market == m1], na.rm = TRUE) * annualize
    sig2 <- sd(front_returns$log_return[front_returns$market == m2], na.rm = TRUE) * annualize

    returns_wide <- front_returns |>
      dplyr::select(.data$date, .data$market, .data$log_return) |>
      tidyr::pivot_wider(names_from = .data$market, values_from = .data$log_return) |>
      stats::na.omit()

    rho <- stats::cor(returns_wide[[m1]], returns_wide[[m2]], use = "pairwise.complete.obs")
    current_spread <- F1 - F2
    spread_range <- seq(current_spread * 0.5, current_spread * 1.5, length.out = 20)

    purrr::map_dfr(spread_range, function(X) {
      result <- tryCatch(
        RTL::spreadOption(
          F1 = F1, F2 = F2, X = X,
          sigma1 = sig1, sigma2 = sig2, rho = rho,
          T2M = 0.25, r = 0.045
        ),
        error = function(e) list(price = NA_real_)
      )

      tibble::tibble(
        spread_level = X,
        payoff = result$price,
        spread_label = paste(m1, "vs", m2)
      )
    })
  }, error = function(e) {
    tibble::tibble(spread_level = numeric(), payoff = numeric(), spread_label = character())
  })

  # --- var_summary: historical VaR/CVaR ---
  var_summary <- front_returns |>
    dplyr::group_by(.data$market) |>
    dplyr::summarise(
      var_95 = stats::quantile(.data$log_return, 0.05, na.rm = TRUE),
      var_99 = stats::quantile(.data$log_return, 0.01, na.rm = TRUE),
      cvar_95 = mean(.data$log_return[.data$log_return <= stats::quantile(.data$log_return, 0.05, na.rm = TRUE)], na.rm = TRUE),
      cvar_99 = mean(.data$log_return[.data$log_return <= stats::quantile(.data$log_return, 0.01, na.rm = TRUE)], na.rm = TRUE),
      .groups = "drop"
    )

  # --- correlation_stress: normal vs high-vol regime correlations ---
  returns_wide_all <- front_returns |>
    dplyr::select(.data$date, .data$market, .data$log_return) |>
    tidyr::pivot_wider(names_from = .data$market, values_from = .data$log_return) |>
    dplyr::arrange(.data$date) |>
    stats::na.omit()

  market_cols <- setdiff(names(returns_wide_all), "date")

  if (length(market_cols) >= 2 && nrow(returns_wide_all) >= 60) {
    ret_mat <- as.matrix(returns_wide_all[, market_cols])
    avg_abs <- rowMeans(abs(ret_mat))
    vol_threshold <- stats::quantile(avg_abs, 0.75)

    normal_idx <- avg_abs <= vol_threshold
    stress_idx <- avg_abs > vol_threshold

    corr_normal <- stats::cor(ret_mat[normal_idx, ], use = "pairwise.complete.obs")
    corr_stress_mat <- stats::cor(ret_mat[stress_idx, ], use = "pairwise.complete.obs")

    correlation_stress <- expand.grid(
      market_x = market_cols, market_y = market_cols, stringsAsFactors = FALSE
    ) |>
      tibble::as_tibble() |>
      dplyr::rowwise() |>
      dplyr::mutate(
        corr_normal = corr_normal[.data$market_x, .data$market_y],
        corr_stress = corr_stress_mat[.data$market_x, .data$market_y]
      ) |>
      dplyr::ungroup()
  } else {
    correlation_stress <- tibble::tibble(
      market_x = character(), market_y = character(),
      corr_normal = numeric(), corr_stress = numeric()
    )
  }

  list(
    price_simulations = price_simulations,
    ou_simulations = ou_simulations,
    stress_scenarios = stress_scenarios,
    spread_option_pnl = spread_option_pnl,
    var_summary = var_summary,
    correlation_stress = correlation_stress
  )
}
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `Rscript -e 'testthat::test_file("tests/testthat/test-calc-scenarios.R")'`
Expected: All PASS.

- [ ] **Step 5: Commit**

```bash
git add R/calc_scenarios.R tests/testthat/test-calc-scenarios.R
git commit -m "feat: add calc_scenarios.R — Monte Carlo, stress testing, VaR"
```

---

## Task 10: Run Full Test Suite and R CMD Check

- [ ] **Step 1: Run all tests**

Run: `Rscript -e 'devtools::test()'`
Expected: All tests pass.

- [ ] **Step 2: Run R CMD check**

Run: `Rscript scripts/check.R`
Expected: No errors, no warnings.

- [ ] **Step 3: Fix any issues found**

If there are R CMD check notes about undeclared global variables, add them to `R/globals.R`. If there are missing imports, add them to DESCRIPTION.

- [ ] **Step 4: Commit fixes if any**

```bash
git add -A
git commit -m "fix: resolve R CMD check notes from calc layer"
```

---

## Summary

| Task | File | What it does |
|------|------|-------------|
| 0 | DESCRIPTION, globals.R | Add dependencies |
| 1 | calc_utils.R | Shared helpers (filter, returns, pivot) |
| 2 | calc_forward_curves.R | Curve snapshot, history, spreads, shape |
| 3 | calc_fundamentals.R | EIA stocks, seasonal bands, utilization |
| 4 | calc_volatility.R | Realized vol, GARCH, vol surface, skew |
| 5 | calc_codynamics.R | Correlations, spreads, betas, PCA |
| 6 | calc_seasonality.R | Seasonal overlay, bands, OU fit |
| 7 | calc_hedging.R | Hedge ratios, rolling beta, effectiveness |
| 8 | calc_overview.R | Price snapshot, perf, drawdowns, vol |
| 9 | calc_scenarios.R | GBM/OU sims, stress, VaR, spread options |
| 10 | Full suite | Integration test + R CMD check |
