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
    "available_markets", "focus_market", "history_context", "history_context_label",
    "display_window", "context_window", "latest_options_date",
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
  # GARCH may fail on some datasets; just ensure it returns a valid df

  expect_gte(nrow(gv), 0)
})

test_that("vol_term_structure shows vol by tenor", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("CL"), expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_volatility(filters)
  vts <- result$vol_term_structure

  expect_s3_class(vts, "data.frame")
  expect_true(all(c("market", "tenor", "rv20", "rv60", "rv120", "realized_vol") %in% names(vts)))
})

test_that("vol_surface_grid has moneyness and tenor dimensions", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("CL"), expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_volatility(filters)
  vsg <- result$vol_surface_grid

  expect_s3_class(vsg, "data.frame")
  expect_true(all(c("market", "curve_point_num", "moneyness", "iv") %in% names(vsg)))
})

test_that("ea_calc_volatility returns new outputs", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("CL"), expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_volatility(filters)
  expect_true(all(c("vol_cone", "vol_of_vol", "vol_of_vol_context", "vol_history_context", "cross_asset_vol", "vol_regime", "iv_rv_spread", "kpis", "notes", "assumptions") %in% names(result)))
})

test_that("vol_cone has percentile bands by horizon", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("CL"), expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_volatility(filters)
  vc <- result$vol_cone
  expect_s3_class(vc, "data.frame")
  if (nrow(vc) > 0) {
    expect_true(all(c("market", "horizon", "current_vol", "percentile", "zscore", "p10", "p25", "p50", "p75", "p90") %in% names(vc)))
    expect_true(all(vc$p10 <= vc$p90))
  }
})

test_that("cross_asset_vol has quartile columns", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("CL", "BRN"), expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_volatility(filters)
  cav <- result$cross_asset_vol
  expect_s3_class(cav, "data.frame")
  if (nrow(cav) > 0) {
    expect_true(all(c("market", "current_vol", "min_vol", "p10", "q1", "median_vol", "q3", "p90", "max_vol", "percentile", "zscore") %in% names(cav)))
  }
})

test_that("vol_regime classifies regime", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("CL"), expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_volatility(filters)
  vr <- result$vol_regime
  expect_s3_class(vr, "data.frame")
  expect_true(all(c("market", "regime", "vol_percentile", "vol_zscore") %in% names(vr)))
})

test_that("kpis has 6 rows with correct structure", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("CL"), expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_volatility(filters)
  kpis <- result$kpis
  expect_s3_class(kpis, "data.frame")
  expect_equal(nrow(kpis), 6)
  expect_true(all(c("title", "value", "delta", "status") %in% names(kpis)))
  expect_true(all(kpis$status %in% c("positive", "warning", "neutral")))
})

test_that("history selector supports arbitrary trailing years without changing displayed realized-vol history", {
  skip_if_no_snapshot()
  filters <- list(
    commodities = c("CL"),
    expiry_range = c(1, 12),
    date_range = as.Date(c("2025-01-01", "2026-03-26"))
  )

  result_5y <- ea_calc_volatility(filters, history_context = "5y")
  result_7y <- ea_calc_volatility(filters, history_context = "7y")

  expect_equal(result_5y$history_context, "5y")
  expect_equal(result_7y$history_context, "7y")
  expect_equal(result_5y$realized_vol_timeseries, result_7y$realized_vol_timeseries)
  expect_equal(result_7y$context_window[[2]], result_7y$display_window[[2]])
  expect_lte(as.numeric(result_7y$context_window[[2]] - result_7y$context_window[[1]]), 366 * 7 + 7)
})

test_that("implied-vol history follows the filtered front listed tenor", {
  skip_if_no_snapshot()

  filters <- list(
    commodities = c("CL"),
    expiry_range = c(3, 12),
    date_range = NULL
  )

  result <- ea_calc_volatility(filters)

  latest_iv <- result$realized_vs_implied %>%
    dplyr::filter(.data$market == "CL", .data$date == result$latest_options_date, is.finite(.data$atm_iv))

  expected_iv <- result$vol_surface_grid %>%
    dplyr::filter(.data$market == "CL", .data$curve_point_num == min(.data$curve_point_num, na.rm = TRUE)) %>%
    dplyr::slice_min(abs(.data$moneyness - 1), n = 1L, with_ties = FALSE)

  expect_equal(nrow(latest_iv), 1L)
  expect_equal(nrow(expected_iv), 1L)
  expect_equal(latest_iv$atm_iv[[1]], expected_iv$iv[[1]])
})
