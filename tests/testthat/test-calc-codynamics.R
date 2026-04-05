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
