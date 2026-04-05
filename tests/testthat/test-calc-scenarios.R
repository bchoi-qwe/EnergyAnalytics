skip_if_no_snapshot <- function() {
  snapshot_dir <- tryCatch(
    ea_latest_data_dir(),
    error = function(...) NA_character_
  )

  if (!is.character(snapshot_dir) || !nzchar(snapshot_dir) || !file.exists(file.path(snapshot_dir, "manifest.json"))) {
    testthat::skip("No local snapshot present. Run scripts/build-data.R after setting API keys.")
  }
}

test_that("ea_calc_scenarios returns all expected outputs", {
  skip_if_no_snapshot()
  filters <- list(
    commodities = c("CL", "BRN"),
    comparison_commodity = "NG",
    expiry_range = c(1, 12),
    date_range = NULL
  )
  shocks <- list(flat = 0, vol = 0, spread = 0)
  result <- ea_calc_scenarios(filters, shocks)

  expect_type(result, "list")
  expect_true(all(c(
    "price_simulations", "ou_simulations", "stress_scenarios",
    "spread_option_pnl", "var_summary", "correlation_stress"
  ) %in% names(result)))
})

test_that("price_simulations returns GBM paths", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("CL"), expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_scenarios(filters, list(flat = 0, vol = 0, spread = 0))
  ps <- result$price_simulations

  expect_s3_class(ps, "data.frame")
  expect_true(all(c("t", "market", "sim_id", "price") %in% names(ps)))
  expect_gt(nrow(ps), 0)
})

test_that("var_summary computes VaR and CVaR", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("CL"), expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_scenarios(filters, list(flat = 0, vol = 0, spread = 0))
  vs <- result$var_summary

  expect_s3_class(vs, "data.frame")
  expect_true(all(c("market", "var_95", "var_99", "cvar_95", "cvar_99") %in% names(vs)))
  # VaR should be negative (loss)
  expect_true(all(vs$var_95 <= 0))
})

test_that("stress_scenarios applies predefined shocks", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("CL", "BRN"), expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_scenarios(filters, list(flat = 5, vol = 10, spread = 2))
  ss <- result$stress_scenarios

  expect_s3_class(ss, "data.frame")
  expect_true(all(c("scenario_label", "market", "shock_pct", "new_price", "pnl_impact") %in% names(ss)))
})

test_that("correlation_stress compares normal vs stress regimes", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("CL", "BRN", "NG"), expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_scenarios(filters, list(flat = 0, vol = 0, spread = 0))
  cs <- result$correlation_stress

  expect_s3_class(cs, "data.frame")
  expect_true(all(c("market_x", "market_y", "corr_normal", "corr_stress") %in% names(cs)))
})

test_that("ea_calc_scenarios returns new outputs", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("CL", "BRN"), expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_scenarios(filters, list(flat = 0, vol = 0, spread = 0))
  expect_true(all(c("historical_analog", "factor_decomposition", "return_distribution",
    "impact_curve", "propagation", "presets", "kpis", "notes", "assumptions") %in% names(result)))
})

test_that("historical_analog has correct schema", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("CL"), expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_scenarios(filters, list(flat = 0, vol = 0, spread = 0))
  ha <- result$historical_analog
  expect_s3_class(ha, "data.frame")
  expect_true(all(c("analog_id", "day", "cumulative_return", "match_date", "match_corr") %in% names(ha)))
})

test_that("presets has 6 rows with correct schema", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("CL"), expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_scenarios(filters, list(flat = 0, vol = 0, spread = 0))
  pr <- result$presets
  expect_s3_class(pr, "data.frame")
  expect_equal(nrow(pr), 6)
  expect_true(all(c("id", "title", "description", "flat", "vol", "spread") %in% names(pr)))
})

test_that("kpis has correct schema", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("CL"), expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_scenarios(filters, list(flat = 0, vol = 0, spread = 0))
  kpis <- result$kpis
  expect_s3_class(kpis, "data.frame")
  expect_true(all(c("title", "value", "delta", "status") %in% names(kpis)))
  expect_equal(nrow(kpis), 5)
  expect_true(all(kpis$status %in% c("positive", "warning", "neutral")))
})

test_that("impact_curve has correct schema", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("CL"), expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_scenarios(filters, list(flat = 10, vol = 0, spread = 0))
  ic <- result$impact_curve
  expect_s3_class(ic, "data.frame")
  expect_true(all(c("tenor", "base", "impact") %in% names(ic)))
  expect_gt(nrow(ic), 0)
  # A positive flat shock should increase impact above base
  expect_true(all(ic$impact >= ic$base * 0.99))  # allowing rounding
})

test_that("return_distribution has correct schema", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("CL"), expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_scenarios(filters, list(flat = 0, vol = 0, spread = 0))
  rd <- result$return_distribution
  expect_s3_class(rd, "data.frame")
  if (nrow(rd) > 0) {
    expect_true(all(c("market", "mean_return", "sd_return", "var_95") %in% names(rd)))
  }
})
