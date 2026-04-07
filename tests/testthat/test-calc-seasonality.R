if (!exists("ea_project_root", mode = "function")) {
  pkgload::load_all(quiet = TRUE)
}

skip_if_no_snapshot <- function() {
  snapshot_dir <- tryCatch(
    ea_latest_data_dir(),
    error = function(...) NA_character_
  )

  if (!is.character(snapshot_dir) || !nzchar(snapshot_dir) || !file.exists(file.path(snapshot_dir, "manifest.json"))) {
    testthat::skip("No local snapshot present. Run scripts/build-data.R after setting API keys.")
  }
}

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
    "mean_reversion", "seasonal_deviation", "display_window",
    "context_window", "anchor_date", "history_context_label"
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

test_that("seasonal_range has seasonal bands", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("CL"), expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_seasonality(filters)
  sr <- result$seasonal_range

  expect_s3_class(sr, "data.frame")
  expect_true(all(c("day_of_year", "market", "avg", "p25", "p75", "min", "max") %in% names(sr)))
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

test_that("ea_calc_seasonality returns all expected outputs including new ones", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("CL", "NG"), expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_seasonality(filters)
  expect_true(all(c("stl_decomposition", "seasonal_spreads", "seasonal_vol",
    "seasonal_hedge_effectiveness", "seasonal_summary", "kpis", "notes", "assumptions") %in% names(result)))
})

test_that("stl_decomposition has correct schema or is empty", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("CL"), expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_seasonality(filters)
  stl <- result$stl_decomposition
  expect_s3_class(stl, "data.frame")
  expect_true(all(c("date", "market", "trend", "seasonal", "remainder") %in% names(stl)))
})

test_that("seasonal_spreads has correct schema", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("CL"), expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_seasonality(filters)
  ss <- result$seasonal_spreads
  expect_s3_class(ss, "data.frame")
  if (nrow(ss) > 0) {
    expect_true(all(c("day_of_year", "market", "avg", "p10", "p25", "p75", "p90", "current_spread", "current_percentile") %in% names(ss)))
  }
})

test_that("kpis has correct schema", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("CL"), expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_seasonality(filters)
  kpis <- result$kpis
  expect_s3_class(kpis, "data.frame")
  expect_true(all(c("title", "value", "delta", "status") %in% names(kpis)))
  expect_equal(nrow(kpis), 5)
  expect_true(all(kpis$status %in% c("positive", "warning", "neutral")))
})

test_that("seasonal_summary has correct schema", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("CL"), expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_seasonality(filters)
  smry <- result$seasonal_summary
  expect_s3_class(smry, "data.frame")
  expect_true(all(c("market", "anomaly_score", "current_percentile", "direction", "days_to_inflection") %in% names(smry)))
})

test_that("seasonality uses display end as anchor and separate history context", {
  skip_if_no_snapshot()

  filters <- list(
    commodities = c("CL"),
    expiry_range = c(1, 12),
    date_range = as.Date(c("2025-01-01", "2026-03-26"))
  )

  result <- ea_calc_seasonality(filters, history_context = "7y")

  expect_equal(result$display_window, as.Date(c("2025-01-02", "2026-03-26")))
  expect_equal(result$anchor_date, as.Date("2026-03-26"))
  expect_equal(result$context_window[[2]], result$anchor_date)
  expect_lt(result$context_window[[1]], result$display_window[[1]])

  if (nrow(result$seasonal_overlay) > 0) {
    overlay_max_day <- max(result$seasonal_overlay$day_of_year, na.rm = TRUE)
    expect_equal(overlay_max_day, as.integer(format(result$anchor_date, "%j")))
  }
})
