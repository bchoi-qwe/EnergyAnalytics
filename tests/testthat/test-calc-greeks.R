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
    "atm_history", "surface_context", "display_window", "context_window",
    "latest_surface_date", "notes", "assumptions") %in% names(result)))
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

test_that("put deltas remain negative after page-level recomputation", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("CL"), expiry_range = c(1, 3), date_range = NULL)
  result <- ea_calc_surface_greeks(filters)
  puts <- result$full_grid %>% dplyr::filter(.data$option_type == "P")

  expect_gt(nrow(puts), 0)
  expect_true(all(puts$delta < 0, na.rm = TRUE))
})

test_that("full_grid is restricted to the latest options snapshot date", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("CL"), expiry_range = c(1, 6), date_range = NULL)
  result <- ea_calc_surface_greeks(filters)

  expect_equal(dplyr::n_distinct(result$full_grid$date), 1)
})

test_that("skew_ratio uses put observations for the OTM put leg", {
  skip_if_no_snapshot()
  filters <- list(commodities = c("CL"), expiry_range = c(1, 6), date_range = NULL)
  result <- ea_calc_surface_greeks(filters)

  expected_puts <- result$full_grid %>%
    dplyr::filter(.data$option_type == "P", abs(.data$moneyness - 0.95) < 0.03) %>%
    dplyr::group_by(.data$market, .data$curve_point_num) %>%
    dplyr::slice_min(abs(.data$moneyness - 0.95), n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(market = .data$market, curve_point_num = .data$curve_point_num, expected_otm_put_iv = .data$iv)

  joined <- result$skew_ratio %>%
    dplyr::inner_join(expected_puts, by = c("market", "curve_point_num"))

  expect_gt(nrow(joined), 0)
  expect_equal(joined$otm_put_iv, joined$expected_otm_put_iv)
})

test_that("options history and snapshot obey the selected display range", {
  skip_if_no_snapshot()

  filters <- list(
    commodities = c("CL"),
    expiry_range = c(1, 6),
    date_range = as.Date(c("2026-03-23", "2026-04-01"))
  )

  result <- ea_calc_surface_greeks(filters, history_context = "full")

  expect_equal(result$display_window, as.Date(c("2026-03-23", "2026-04-01")))
  expect_equal(result$latest_surface_date, as.Date("2026-04-01"))
  expect_equal(unique(result$full_grid$date), as.Date("2026-04-01"))

  if (nrow(result$atm_history) > 0) {
    expect_true(all(result$atm_history$date >= result$display_window[[1]]))
    expect_true(all(result$atm_history$date <= result$display_window[[2]]))
  }
})

test_that("surface_context exposes IV and skew history context", {
  skip_if_no_snapshot()

  filters <- list(commodities = c("CL"), expiry_range = c(1, 6), date_range = NULL)
  result <- ea_calc_surface_greeks(filters, history_context = "full")

  expect_s3_class(result$surface_context, "data.frame")
  expect_true(all(c("market", "atm_iv", "iv_percentile", "iv_zscore", "skew_ratio", "skew_percentile", "skew_zscore") %in% names(result$surface_context)))
})

test_that("Black-76 engine matches closed-form price and primary greeks", {
  black76_reference <- function(F, K, r, T, sigma, is_call = TRUE) {
    d1 <- (log(F / K) + 0.5 * sigma^2 * T) / (sigma * sqrt(T))
    d2 <- d1 - sigma * sqrt(T)
    df <- exp(-r * T)
    nd1 <- stats::dnorm(d1)
    Nd1 <- stats::pnorm(d1)
    Nd2 <- stats::pnorm(d2)

    premium <- if (is_call) {
      df * (F * Nd1 - K * Nd2)
    } else {
      df * (K * stats::pnorm(-d2) - F * stats::pnorm(-d1))
    }

    c(
      premium = premium,
      delta = if (is_call) df * Nd1 else df * (Nd1 - 1),
      gamma = df * nd1 / (F * sigma * sqrt(T)),
      vega = df * F * nd1 * sqrt(T),
      rho = -T * premium
    )
  }

  grid <- expand.grid(
    F = c(50, 80, 100),
    K = c(50, 90, 100),
    r = c(0.01, 0.04),
    T = c(0.25, 1),
    sigma = c(0.2, 0.5),
    is_call = c(TRUE, FALSE),
    KEEP.OUT.ATTRS = FALSE
  )

  calc <- black76_greeks_vectorised(grid$F, grid$K, grid$r, grid$T, grid$sigma, grid$is_call)

  for (metric in c("premium", "delta", "gamma", "vega", "rho")) {
    reference <- mapply(
      function(F, K, r, T, sigma, is_call) {
        black76_reference(F, K, r, T, sigma, is_call)[[metric]]
      },
      grid$F,
      grid$K,
      grid$r,
      grid$T,
      grid$sigma,
      grid$is_call
    )

    tolerance <- switch(
      metric,
      premium = 1e-10,
      delta = 5e-4,
      gamma = 1e-4,
      vega = 2e-2,
      rho = 1e-6
    )

    expect_lte(max(abs(calc[[metric]] - as.numeric(reference))), tolerance)
  }
})

test_that("OU half-life outputs are derived from theta instead of the raw RTL helper field", {
  skip_if_no_snapshot()

  filters <- list(commodities = c("CL"), expiry_range = c(1, 12), date_range = NULL)
  seasonality <- ea_calc_seasonality(filters)

  valid_half_life <- seasonality$mean_reversion %>%
    dplyr::filter(is.finite(.data$theta), .data$theta > 0, is.finite(.data$half_life))

  if (nrow(valid_half_life) > 0L) {
    expect_equal(
      valid_half_life$half_life,
      ea_ou_half_life_days(valid_half_life$theta),
      tolerance = 1e-8
    )
  }
})

test_that("term_greeks uses the nearest listed ATM row for each available tenor", {
  skip_if_no_snapshot()

  filters <- list(commodities = c("CL", "HO", "NG", "RB"), expiry_range = c(1, 12), date_range = NULL)
  result <- ea_calc_surface_greeks(filters)

  expect_true(all(c("moneyness", "atm_gap", "atm_iv") %in% names(result$term_greeks)))

  expected_pairs <- result$full_grid %>%
    dplyr::distinct(.data$market, .data$curve_point_num) %>%
    dplyr::arrange(.data$market, .data$curve_point_num)

  actual_pairs <- result$term_greeks %>%
    dplyr::distinct(.data$market, .data$curve_point_num) %>%
    dplyr::arrange(.data$market, .data$curve_point_num)

  expect_equal(actual_pairs, expected_pairs)
})
