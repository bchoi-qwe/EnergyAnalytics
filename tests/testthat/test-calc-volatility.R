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
