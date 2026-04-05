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

test_that("ea_calc_hedging returns all expected outputs", {
  skip_if_no_snapshot()
  filters <- list(
    commodities = c("CL", "BRN", "HO"),
    comparison_commodity = "CL",
    expiry_range = c(1, 12),
    date_range = NULL,
    rolling_window = "63D"
  )
  result <- ea_calc_hedging(filters)

  expect_type(result, "list")
  expect_true(all(c(
    "hedge_ratios", "rolling_beta", "hedge_effectiveness",
    "residual_timeseries", "ou_fit", "cross_hedge_matrix"
  ) %in% names(result)))
})

test_that("hedge_ratios returns OLS betas", {
  skip_if_no_snapshot()
  filters <- list(
    commodities = c("BRN", "HO"),
    comparison_commodity = "CL",
    expiry_range = c(1, 12),
    date_range = NULL
  )
  result <- ea_calc_hedging(filters)
  hr <- result$hedge_ratios

  expect_s3_class(hr, "data.frame")
  expect_true(all(c("market", "beta", "r_squared", "std_error") %in% names(hr)))
  expect_gt(nrow(hr), 0)
})

test_that("rolling_beta has time dimension", {
  skip_if_no_snapshot()
  filters <- list(
    commodities = c("BRN"),
    comparison_commodity = "CL",
    expiry_range = c(1, 12),
    date_range = NULL,
    rolling_window = "63D"
  )
  result <- ea_calc_hedging(filters)
  rb <- result$rolling_beta

  expect_s3_class(rb, "data.frame")
  expect_true(all(c("date", "market", "beta", "r_squared") %in% names(rb)))
  expect_gt(nrow(rb), 0)
})

test_that("hedge_effectiveness computes vol reduction", {
  skip_if_no_snapshot()
  filters <- list(
    commodities = c("BRN"),
    comparison_commodity = "CL",
    expiry_range = c(1, 12),
    date_range = NULL
  )
  result <- ea_calc_hedging(filters)
  he <- result$hedge_effectiveness

  expect_s3_class(he, "data.frame")
  expect_true(all(c("market", "unhedged_vol", "hedged_vol", "vol_reduction_pct") %in% names(he)))
})

test_that("ou_fit returns OU parameters on residuals", {
  skip_if_no_snapshot()
  filters <- list(
    commodities = c("BRN"),
    comparison_commodity = "CL",
    expiry_range = c(1, 12),
    date_range = NULL
  )
  result <- ea_calc_hedging(filters)
  ou <- result$ou_fit

  expect_s3_class(ou, "data.frame")
  expect_true(all(c("market", "theta", "mu", "sigma", "half_life") %in% names(ou)))
})

test_that("ea_calc_hedging returns new outputs", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("BRN", "HO"), comparison_commodity = "CL",
                  expiry_range = c(1, 12), date_range = NULL, rolling_window = "63D")
  result <- ea_calc_hedging(filters)
  expect_true(all(c("per_tenor_ratios", "ratio_stability", "hedge_cost",
                    "stress_period_comparison", "residual_with_bands",
                    "kpis", "notes", "assumptions") %in% names(result)))
})

test_that("per_tenor_ratios has beta at each tenor", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("BRN"), comparison_commodity = "CL",
                  expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_hedging(filters)
  ptr <- result$per_tenor_ratios
  expect_s3_class(ptr, "data.frame")
  if (nrow(ptr) > 0) {
    expect_true(all(c("market", "tenor", "beta", "r_squared") %in% names(ptr)))
    expect_gt(length(unique(ptr$tenor)), 1)
  }
})

test_that("ratio_stability returns beta SD per market", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("BRN", "HO"), comparison_commodity = "CL",
                  expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_hedging(filters)
  rs <- result$ratio_stability
  expect_s3_class(rs, "data.frame")
  expect_true(all(c("market", "beta_sd") %in% names(rs)))
})

test_that("kpis has 6 rows", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("BRN"), comparison_commodity = "CL",
                  expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_hedging(filters)
  kpis <- result$kpis
  expect_s3_class(kpis, "data.frame")
  expect_equal(nrow(kpis), 6)
  expect_true(all(c("title", "value", "delta", "status") %in% names(kpis)))
  expect_true(all(kpis$status %in% c("positive", "warning", "neutral")))
})

test_that("hedge_cost has correct schema", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("BRN"), comparison_commodity = "CL",
                  expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_hedging(filters)
  hc <- result$hedge_cost
  expect_s3_class(hc, "data.frame")
  expect_true(all(c("market", "roll_cost", "basis_risk_cost") %in% names(hc)))
})

test_that("residual_with_bands has correct schema", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("BRN"), comparison_commodity = "CL",
                  expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_hedging(filters)
  rwb <- result$residual_with_bands
  expect_s3_class(rwb, "data.frame")
  expect_true(all(c("date", "market", "residual", "ou_mu",
                    "band_1_lo", "band_1_hi", "band_2_lo", "band_2_hi") %in% names(rwb)))
})

test_that("stress_period_comparison has correct schema", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("BRN"), comparison_commodity = "CL",
                  expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_hedging(filters)
  spc <- result$stress_period_comparison
  expect_s3_class(spc, "data.frame")
  expect_true(all(c("market", "r2_normal", "r2_stress") %in% names(spc)))
})
