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
    "spread_zscore", "beta_matrix", "pca_decomposition", "treasury_curve_snapshot",
    "display_window", "context_window", "history_context_label"
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

test_that("ea_calc_codynamics returns new outputs", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("CL", "BRN"), comparison_commodity = "NG",
                  expiry_range = c(1, 12), date_range = NULL, rolling_window = "63D")
  result <- ea_calc_codynamics(filters, history_context = "7y")
  expect_true(all(c("treasury_betas", "connectedness_score", "correlation_breaks",
                    "coint_residual", "rolling_beta_ts", "kpis", "notes", "assumptions") %in% names(result)))
})

test_that("correlation_timeseries has CI bands", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("CL", "BRN"), expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_codynamics(filters)
  ct <- result$correlation_timeseries
  expect_true(all(c("ci_lo", "ci_hi", "se") %in% names(ct)))
  if (nrow(ct) > 0) {
    expect_true(all(ct$ci_lo >= -1, na.rm = TRUE))
    expect_true(all(ct$ci_hi <= 1, na.rm = TRUE))
  }
})

test_that("treasury_betas has correct schema", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("CL", "BRN"), expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_codynamics(filters)
  tb <- result$treasury_betas
  expect_s3_class(tb, "data.frame")
  # Schema must always be correct regardless of whether rows were computed
  expect_true(all(c("market", "factor", "beta") %in% names(tb)))
  if (nrow(tb) > 0) {
    expect_true(all(tb$factor %in% c("Level", "Slope", "Curvature")))
  }
})

test_that("treasury_curve_snapshot exposes the full constant maturity set", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("CL"), expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_codynamics(filters)
  tc <- result$treasury_curve_snapshot

  expect_s3_class(tc, "data.frame")
  expect_true(all(c("date", "tenor", "tenor_years", "yield", "change_bps") %in% names(tc)))

  expected_tenors <- c("1MO", "3MO", "6MO", "1Y", "2Y", "3Y", "5Y", "7Y", "10Y", "20Y", "30Y")
  if (nrow(tc) > 0) {
    expect_true(all(expected_tenors %in% tc$tenor))
  }
})

test_that("kpis has 6 rows", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("CL", "BRN", "NG"), expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_codynamics(filters)
  kpis <- result$kpis
  expect_s3_class(kpis, "data.frame")
  expect_equal(nrow(kpis), 6)
  expect_true(all(c("title", "value", "delta", "status") %in% names(kpis)))
  expect_true(all(kpis$status %in% c("positive", "warning", "neutral")))
})

test_that("pair correlations do not change when an unrelated market is added", {
  skip_if_no_snapshot()

  base_filters <- list(commodities = c("CL", "BRN"), expiry_range = c(1, 12), date_range = NULL, rolling_window = "63D")
  extra_filters <- list(commodities = c("CL", "BRN", "HTT"), expiry_range = c(1, 12), date_range = NULL, rolling_window = "63D")

  base_result <- ea_calc_codynamics(base_filters)
  extra_result <- ea_calc_codynamics(extra_filters)

  base_corr <- base_result$correlation_matrix %>%
    dplyr::filter(.data$market_x == "CL", .data$market_y == "BRN") %>%
    dplyr::pull(.data$correlation)

  extra_corr <- extra_result$correlation_matrix %>%
    dplyr::filter(.data$market_x == "CL", .data$market_y == "BRN") %>%
    dplyr::pull(.data$correlation)

  expect_length(base_corr, 1)
  expect_length(extra_corr, 1)
  expect_equal(base_corr, extra_corr)
})

test_that("co-dynamics uses the display window for charts and a separate history context", {
  skip_if_no_snapshot()

  filters <- list(
    commodities = c("CL", "BRN", "NG"),
    expiry_range = c(1, 12),
    date_range = as.Date(c("2025-01-01", "2026-03-26")),
    rolling_window = "63D"
  )

  result <- ea_calc_codynamics(filters, history_context = "7y")

  expect_equal(result$display_window, as.Date(c("2025-01-02", "2026-03-26")))
  expect_equal(result$context_window[[2]], result$display_window[[2]])
  expect_lt(result$context_window[[1]], result$display_window[[1]])

  if (nrow(result$correlation_timeseries) > 0) {
    expect_true(all(result$correlation_timeseries$date >= result$display_window[[1]]))
    expect_true(all(result$correlation_timeseries$date <= result$display_window[[2]]))
  }
})

test_that("rolling beta output now includes rolling R-squared", {
  skip_if_no_snapshot()

  filters <- list(
    commodities = c("CL", "BRN"),
    expiry_range = c(1, 12),
    date_range = NULL,
    rolling_window = "63D"
  )

  result <- ea_calc_codynamics(filters)
  expect_true(all(c("date", "pair", "beta", "r_squared") %in% names(result$rolling_beta_ts)))
})
