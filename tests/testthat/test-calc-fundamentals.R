if (!exists("ea_project_root", mode = "function")) {
  candidate_roots <- c(".", "..", "../..")
  source_root <- candidate_roots[file.exists(file.path(candidate_roots, "R", "data_foundation.R"))][1]

  if (is.na(source_root)) {
    stop("Could not locate R/data_foundation.R for direct test execution.", call. = FALSE)
  }

  options(EnergyAnalytics.project_root = normalizePath(source_root, winslash = "/", mustWork = TRUE))
  source(file.path(source_root, "R", "data_foundation.R"), local = FALSE)
  source(file.path(source_root, "R", "ui_helpers.R"), local = FALSE)
  source(file.path(source_root, "R", "app_mock_data.R"), local = FALSE)
  source(file.path(source_root, "R", "calc_utils.R"), local = FALSE)
  source(file.path(source_root, "R", "calc_fundamentals.R"), local = FALSE)
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
  sd_df <- result$stocks_deviation

  expect_s3_class(sd_df, "data.frame")
  expect_true(all(c("date", "product", "deviation", "deviation_pct") %in% names(sd_df)))
})
