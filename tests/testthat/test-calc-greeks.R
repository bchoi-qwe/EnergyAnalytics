if (!exists("ea_project_root", mode = "function")) {
  pkgload::load_all(quiet = TRUE)
}

skip_if_no_snapshot <- function() {
  snapshot_dir <- tryCatch(ea_latest_data_dir(), error = function(...) NA_character_)
  if (!is.character(snapshot_dir) || !nzchar(snapshot_dir) || !file.exists(file.path(snapshot_dir, "manifest.json"))) {
    testthat::skip("No local snapshot present.")
  }
}

test_that("ea_calc_surface_greeks returns all expected outputs", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("CL"), expiry_range = c(1, 6), date_range = NULL)
  result <- ea_calc_surface_greeks(filters)
  expect_type(result, "list")
  expect_true(all(c("kpis", "full_grid", "cross_greeks_vanna", "cross_greeks_charm",
    "term_greeks", "strike_profile", "greeks_concentration", "pnl_grid", "skew_ratio",
    "notes", "assumptions") %in% names(result)))
})

test_that("greeks_concentration has correct schema", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("CL"), expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_surface_greeks(filters)
  gc <- result$greeks_concentration
  expect_s3_class(gc, "data.frame")
  expect_true(all(c("market", "label", "tenor_bucket", "delta", "gamma", "vega", "theta") %in% names(gc)))
  expect_gt(nrow(gc), 0)
})

test_that("pnl_grid has correct schema", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("CL"), expiry_range = c(1, 6), date_range = NULL)
  result <- ea_calc_surface_greeks(filters)
  pg <- result$pnl_grid
  expect_s3_class(pg, "data.frame")
  expect_true(all(c("spot_shock", "vol_shock", "pnl") %in% names(pg)))
  expect_gt(nrow(pg), 0)
})

test_that("skew_ratio has correct schema", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("CL"), expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_surface_greeks(filters)
  sr <- result$skew_ratio
  expect_s3_class(sr, "data.frame")
  expect_true(all(c("market", "curve_point_num", "atm_iv", "otm_put_iv", "skew_ratio") %in% names(sr)))
})

test_that("kpis has correct schema", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("CL"), expiry_range = c(1, 6), date_range = NULL)
  result <- ea_calc_surface_greeks(filters)
  kpis <- result$kpis
  expect_s3_class(kpis, "data.frame")
  expect_true(all(c("title", "value", "delta", "status") %in% names(kpis)))
})

test_that("full_grid has all greek columns", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("CL"), expiry_range = c(1, 3), date_range = NULL)
  result <- ea_calc_surface_greeks(filters)
  fg <- result$full_grid
  expect_s3_class(fg, "data.frame")
  expect_true(all(c("market", "forward", "strike", "moneyness", "iv",
    "delta", "gamma", "vega", "theta", "vanna", "charm", "speed", "zomma") %in% names(fg)))
  expect_gt(nrow(fg), 0)
})
