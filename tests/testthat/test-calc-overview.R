skip_if_no_snapshot <- function() {
  snapshot_dir <- tryCatch(
    ea_latest_data_dir(),
    error = function(...) NA_character_
  )

  if (!is.character(snapshot_dir) || !nzchar(snapshot_dir) || !file.exists(file.path(snapshot_dir, "manifest.json"))) {
    testthat::skip("No local snapshot present. Run scripts/build-data.R after setting API keys.")
  }
}

test_that("ea_calc_overview returns all expected outputs", {
  skip_if_no_snapshot()
  filters <- list(
    commodities = c("CL", "BRN", "NG"),
    comparison_commodity = "HO",
    expiry_range = c(1, 12),
    date_range = NULL,
    rolling_window = "63D"
  )
  result <- ea_calc_overview(filters)

  expect_type(result, "list")
  expect_true(all(c(
    "price_snapshot", "relative_performance", "curve_structure_summary",
    "correlation_snapshot", "spread_monitor", "vol_snapshot", "drawdown_summary"
  ) %in% names(result)))
})

test_that("price_snapshot has current prices and changes", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("CL", "BRN"), expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_overview(filters)
  ps <- result$price_snapshot

  expect_s3_class(ps, "data.frame")
  expect_true(all(c("market", "price", "change", "change_pct") %in% names(ps)))
  expect_true(all(ps$market %in% c("CL", "BRN")))
})

test_that("relative_performance is indexed to 100", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("CL", "BRN"), expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_overview(filters)
  rp <- result$relative_performance

  expect_s3_class(rp, "data.frame")
  expect_true(all(c("date", "market", "indexed_value") %in% names(rp)))
})

test_that("drawdown_summary computes drawdowns", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("CL"), expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_overview(filters)
  dd <- result$drawdown_summary

  expect_s3_class(dd, "data.frame")
  expect_true(all(c("market", "drawdown_pct", "days_from_peak") %in% names(dd)))
})

test_that("vol_snapshot includes percentile rank", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("CL"), expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_overview(filters)
  vs <- result$vol_snapshot

  expect_s3_class(vs, "data.frame")
  expect_true(all(c("market", "realized_vol_20d", "vol_percentile") %in% names(vs)))
})

test_that("ea_calc_overview returns new outputs", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("CL", "BRN"), expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_overview(filters)
  expect_true(all(c("market_snapshot_table", "top_anomalies", "upcoming_events",
    "kpis", "notes", "assumptions") %in% names(result)))
})

test_that("market_snapshot_table has correct schema", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("CL", "BRN"), expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_overview(filters)
  mst <- result$market_snapshot_table
  expect_s3_class(mst, "data.frame")
  expect_true(all(c("market", "label", "price", "chg_1d", "pct_1d", "percentile_52w") %in% names(mst)))
  expect_gt(nrow(mst), 0)
})

test_that("top_anomalies has correct schema", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("CL", "BRN"), expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_overview(filters)
  ta <- result$top_anomalies
  expect_s3_class(ta, "data.frame")
  expect_true(all(c("label", "anomaly_type", "zscore") %in% names(ta)))
  expect_lte(nrow(ta), 5)
})

test_that("upcoming_events has correct schema", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("CL"), expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_overview(filters)
  ue <- result$upcoming_events
  expect_s3_class(ue, "data.frame")
  expect_true(all(c("event_date", "event_name", "event_type") %in% names(ue)))
})

test_that("kpis has correct schema with 6 rows", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("CL", "BRN"), expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_overview(filters)
  kpis <- result$kpis
  expect_s3_class(kpis, "data.frame")
  expect_true(all(c("title", "value", "delta", "status") %in% names(kpis)))
  expect_equal(nrow(kpis), 6)
  expect_true(all(kpis$status %in% c("positive", "warning", "neutral")))
})
