if (!exists("ea_project_root", mode = "function")) {
  candidate_roots <- c(".", "..", "../..")
  source_root <- candidate_roots[file.exists(file.path(candidate_roots, "R", "data_foundation.R"))][1]

  if (is.na(source_root)) {
    stop("Could not locate R/data_foundation.R for direct test execution.", call. = FALSE)
  }

  options(EnergyAnalytics.project_root = normalizePath(source_root, winslash = "/", mustWork = TRUE))
  source(file.path(source_root, "R", "data_foundation.R"), local = FALSE)
  source(file.path(source_root, "R", "ui_helpers.R"), local = FALSE)
  source(file.path(source_root, "R", "calc_utils.R"), local = FALSE)
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
  expect_true("01" %in% names(result) || "1" %in% names(result))
})

test_that("history context helpers support arbitrary trailing years", {
  expect_equal(ea_normalize_history_context("7y"), "7y")
  expect_equal(ea_normalize_history_context(12), "12y")
  expect_equal(ea_normalize_history_context(list(mode = "trailing", years = 4)), "4y")
  expect_equal(ea_normalize_history_context(list(mode = "full", years = 4)), "full")
  expect_equal(ea_history_context_label("7y"), "7Y")
  expect_equal(ea_history_context_label("full"), "Full")
  expect_equal(
    ea_history_context_start_date(as.Date("2026-03-26"), "7y"),
    as.Date("2026-03-26") - 365L * 7L
  )
})

test_that("OU helper functions convert annualized parameters to display units", {
  expect_equal(ea_ou_stationary_sd(theta = 8, sigma = 4), 1, tolerance = 1e-12)
  expect_equal(ea_ou_half_life_days(theta = log(2) * 252), 1, tolerance = 1e-12)
  expect_true(is.na(ea_ou_stationary_sd(theta = NA_real_, sigma = 2)))
  expect_true(is.na(ea_ou_stationary_sd(theta = 0, sigma = 2)))
  expect_true(is.na(ea_ou_half_life_days(theta = -1)))
})
