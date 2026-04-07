if (!exists("ea_project_root", mode = "function")) {
  candidate_roots <- c(".", "..", "../..")
  source_root <- candidate_roots[file.exists(file.path(candidate_roots, "R", "data_foundation.R"))][1]

  if (is.na(source_root)) {
    stop("Could not locate R/data_foundation.R for direct test execution.", call. = FALSE)
  }

  options(EnergyAnalytics.project_root = normalizePath(source_root, winslash = "/", mustWork = TRUE))
  source(file.path(source_root, "R", "data_foundation.R"), local = FALSE)
  source(file.path(source_root, "R", "app_utils.R"), local = FALSE)
  source(file.path(source_root, "R", "ui_helpers.R"), local = FALSE)
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
  expected_names <- c(
    "available_markets", "focus_market", "history_context", "history_context_label",
    "display_window", "context_window",
    "curve_snapshot", "curve_history", "curve_change_heatmap",
    "calendar_spreads", "calendar_spread_strip", "prompt_spread_ts",
    "roll_yield", "curve_pca", "treasury_overlay", "structure_summary",
    "kpis", "notes", "assumptions"
  )
  expect_true(all(expected_names %in% names(result)))
  expect_false("curve_shape_metrics" %in% names(result))
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
  expect_true(all(cs$price > 0))
})

test_that("curve_history returns multiple snapshots", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("CL"), expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_forward_curves(filters)
  ch <- result$curve_history

  expect_s3_class(ch, "data.frame")
  expect_true(all(c("snapshot_label", "snapshot_date", "curve_point_num", "price") %in% names(ch)))
  expect_true("NOW" %in% ch$snapshot_label)
  expect_gt(length(unique(ch$snapshot_label)), 1)
})

test_that("curve_change_heatmap includes market, level, and change fields", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("CL"), expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_forward_curves(filters)
  hm <- result$curve_change_heatmap

  expect_s3_class(hm, "data.frame")
  expect_true(all(c("market", "date", "curve_point_num", "value", "change") %in% names(hm)))
  expect_true(all(hm$market == "CL"))
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

test_that("calendar_spread_strip has correct columns and z-scores", {
  skip_if_no_snapshot()
  filters <- list(commodities = "CL", expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_forward_curves(filters)

  strip <- result$calendar_spread_strip
  expect_s3_class(strip, "data.frame")
  expect_true(all(c("market", "spread_label", "front_tenor", "current_spread", "zscore", "percentile", "rich_cheap") %in% names(strip)))
  if (nrow(strip) > 0) {
    expect_true(all(is.finite(strip$current_spread)))
    expect_true(all(strip$market == "CL"))
    expect_true(all(strip$rich_cheap %in% c("Rich", "Cheap", "Neutral")))
  }
})

test_that("prompt_spread_ts has correct columns and percentile bands when enough data", {
  skip_if_no_snapshot()
  filters <- list(commodities = "CL", expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_forward_curves(filters)

  ts_data <- result$prompt_spread_ts
  expect_s3_class(ts_data, "data.frame")
  expect_true(all(c("date", "market", "spread", "spread_mean", "spread_sd", "zscore", "p5", "p25", "p75", "p95") %in% names(ts_data)))
  if (nrow(ts_data) > 0) {
    expect_true(all(ts_data$market == "CL"))
  }
})

test_that("roll_yield has correct structure and finite values", {
  skip_if_no_snapshot()
  filters <- list(commodities = "CL", expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_forward_curves(filters)

  ry <- result$roll_yield
  expect_s3_class(ry, "data.frame")
  expect_true(all(c("market", "tenor_pair", "front_tenor", "front_price",
                    "back_price", "spread", "roll_yield_ann") %in% names(ry)))
  if (nrow(ry) > 0) {
    expect_true(all(is.finite(ry$roll_yield_ann)))
    expect_true(all(is.finite(ry$front_price)))
    expect_true(all(is.finite(ry$back_price)))
    expect_true(all(ry$market == "CL"))
  }
})

test_that("curve_pca returns loadings and var_explained", {
  skip_if_no_snapshot()
  filters <- list(commodities = "CL", expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_forward_curves(filters)

  pca <- result$curve_pca
  expect_type(pca, "list")
  expect_true(all(c("loadings", "var_explained") %in% names(pca)))
  expect_s3_class(pca$loadings, "data.frame")
  expect_s3_class(pca$var_explained, "data.frame")
  expect_true(all(c("tenor", "PC1", "PC2", "PC3") %in% names(pca$loadings)))
  expect_true(all(c("component", "proportion") %in% names(pca$var_explained)))

  if (nrow(pca$var_explained) > 0) {
    expect_true(all(pca$var_explained$proportion >= 0 & pca$var_explained$proportion <= 1))
  }
})

test_that("treasury_overlay has correct columns", {
  skip_if_no_snapshot()
  filters <- list(commodities = "CL", expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_forward_curves(filters)

  tov <- result$treasury_overlay
  expect_s3_class(tov, "data.frame")
  expect_true(all(c("date", "market", "cmdty_spread", "slope_2s10s") %in% names(tov)))
})

test_that("structure_summary has enhanced columns with regime classification", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("CL", "BRN"), expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_forward_curves(filters)

  ss <- result$structure_summary
  expect_s3_class(ss, "data.frame")
  expected_cols <- c("market", "snapshot_date", "m1_price", "m1_m2", "m1_m6", "m1_m12",
                     "slope", "curvature", "regime", "regime_share", "m1_m2_percentile",
                     "m1_m12_percentile", "prompt_zscore", "roll_yield_ann", "prompt_signal", "curve_signal")
  expect_true(all(expected_cols %in% names(ss)))
  expect_true(nrow(ss) > 0)
  valid_regimes <- c("Steep Backwardation", "Backwardation", "Super Contango",
                     "Humped Contango", "Contango", "Inverted Belly", "Flat")
  expect_true(all(ss$regime %in% valid_regimes))
  expect_true(all(ss$m1_price > 0))
})

test_that("kpis has 6 rows with correct columns and valid status", {
  skip_if_no_snapshot()
  filters <- list(commodities = "CL", expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_forward_curves(filters)

  kpis <- result$kpis
  expect_s3_class(kpis, "data.frame")
  expect_true(all(c("title", "value", "delta", "status") %in% names(kpis)))
  expect_equal(nrow(kpis), 6)
  expect_true(all(kpis$status %in% c("positive", "warning", "neutral")))
  expect_equal(kpis$title, c("M1 Price", "Prompt Spread", "Prompt Z",
                              "Regime", "Curve Percentile", "Roll Yield"))
})

test_that("notes and assumptions are non-empty character vectors", {
  skip_if_no_snapshot()
  filters <- list(commodities = "CL", expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_forward_curves(filters)

  expect_type(result$notes, "character")
  expect_type(result$assumptions, "character")
  expect_true(length(result$notes) > 0)
  expect_true(length(result$assumptions) > 0)
})

test_that("history selector supports arbitrary trailing years without changing snapshot", {
  skip_if_no_snapshot()
  filters <- list(
    commodities = "CL",
    expiry_range = c(1, 12),
    date_range = as.Date(c("2025-01-01", "2026-03-26"))
  )

  result_5y <- ea_calc_forward_curves(filters, history_context = "5y")
  result_7y <- ea_calc_forward_curves(filters, history_context = "7y")

  expect_equal(result_5y$history_context, "5y")
  expect_equal(result_7y$history_context, "7y")
  expect_equal(result_5y$curve_snapshot, result_7y$curve_snapshot)
  expect_equal(result_7y$context_window[[2]], result_7y$display_window[[2]])
  expect_lte(as.numeric(result_7y$context_window[[2]] - result_7y$context_window[[1]]), 366 * 7 + 7)
})

test_that("calendar spreads and structure metrics follow the filtered front tenor", {
  skip_if_no_snapshot()

  filters <- list(
    commodities = "CL",
    expiry_range = c(3, 12),
    date_range = NULL
  )

  result <- ea_calc_forward_curves(filters)

  expect_gt(nrow(result$calendar_spreads), 0)
  expect_true("M1-M2" %in% result$calendar_spreads$spread_label)

  latest_curve <- result$curve_snapshot %>%
    dplyr::filter(.data$market == "CL") %>%
    dplyr::arrange(.data$curve_point_num)

  summary_row <- result$structure_summary %>%
    dplyr::filter(.data$market == "CL")

  front_3 <- latest_curve$price[latest_curve$curve_point_num == 3][[1]]
  front_4 <- latest_curve$price[latest_curve$curve_point_num == 4][[1]]
  front_8 <- latest_curve$price[latest_curve$curve_point_num == 8][[1]]

  expect_equal(summary_row$m1_price[[1]], front_3)
  expect_equal(summary_row$m1_m2[[1]], front_3 - front_4)
  expect_equal(summary_row$m1_m6[[1]], front_3 - front_8)
})
