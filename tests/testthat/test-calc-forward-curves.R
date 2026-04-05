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
  source(file.path(source_root, "R", "calc_forward_curves.R"), local = FALSE)
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
