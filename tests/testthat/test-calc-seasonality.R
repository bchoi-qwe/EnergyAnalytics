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
