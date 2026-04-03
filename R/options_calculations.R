# ---- Options Greeks Calculation Pipeline ----
# Bridges canonical Feather datasets to the Rcpp Black-76 engine.

#' @useDynLib EnergyAnalytics, .registration = TRUE
#' @importFrom Rcpp sourceCpp
NULL

ea_interpolate_risk_free <- function(ust_curve, days_to_expiry) {
  ust_latest <- ust_curve |>
    dplyr::filter(date == max(date)) |>
    dplyr::arrange(curve_point_num) |>
    dplyr::select(curve_point_num, value)

  tenors_years <- ust_latest$curve_point_num
  rates <- ust_latest$value / 100

  vapply(
    days_to_expiry,
    function(dte) {
      t_years <- dte / 365.25
      if (t_years <= min(tenors_years)) return(rates[1])
      if (t_years >= max(tenors_years)) return(rates[length(rates)])
      stats::approx(x = tenors_years, y = rates, xout = t_years, rule = 2)$y
    },
    numeric(1)
  )
}

ea_generate_vol_surface <- function(markets, forwards_df, catalog) {
  moneyness_grid <- c(0.80, 0.85, 0.90, 0.95, 1.00, 1.05, 1.10, 1.15, 1.20)

  beta1 <- -0.15
  beta2 <- 0.80

  purrr::map_dfr(markets, function(mkt) {
    mkt_forwards <- forwards_df |>
      dplyr::filter(market == mkt) |>
      dplyr::arrange(curve_point_num)

    if (nrow(mkt_forwards) == 0L) return(tibble::tibble())

    base_vol <- ea_market_base_vol(mkt)
    alpha <- -0.05

    purrr::map_dfr(seq_len(nrow(mkt_forwards)), function(j) {
      fwd <- mkt_forwards[j, ]
      tenor_num <- fwd$curve_point_num
      fwd_price <- fwd$value

      term_adj <- (tenor_num / 12)^alpha

      purrr::map_dfr(moneyness_grid, function(m) {
        strike <- fwd_price * m
        log_m <- log(m)
        skew_adj <- 1.0 + beta1 * log_m + beta2 * log_m^2
        iv <- base_vol * term_adj * skew_adj
        iv <- max(iv, 0.01)

        tibble::tibble(
          market = mkt,
          label = ea_market_labels(mkt, catalog),
          curve_point = fwd$curve_point,
          curve_point_num = tenor_num,
          forward = fwd_price,
          strike = strike,
          moneyness = m,
          iv = iv
        )
      })
    })
  })
}

ea_calc_surface_greeks <- function(filters) {
  catalog <- ea_market_catalog()
  markets <- ea_coalesce(filters$commodities, ea_global_filter_defaults(catalog)$commodities)
  labels <- stats::setNames(catalog$label, catalog$market)

  expiry_lo <- ea_coalesce(filters$expiry_range[1], 1L)
  expiry_hi <- ea_coalesce(filters$expiry_range[2], 12L)

  commodity_curves <- tryCatch(
    ea_load_dataset("commodity_curve_long"),
    error = function(e) NULL
  )
  ust_curve <- tryCatch(
    ea_load_dataset("ust_curve_long"),
    error = function(e) NULL
  )
  expiry_meta <- tryCatch(
    ea_load_dataset("contract_expiry_metadata"),
    error = function(e) NULL
  )

  if (is.null(commodity_curves) || is.null(ust_curve)) {
    return(ea_greeks_fallback(markets, labels))
  }

  latest_date <- max(commodity_curves$date, na.rm = TRUE)
  forwards_df <- commodity_curves |>
    dplyr::filter(
      date == latest_date,
      market %in% markets,
      curve_point_num >= expiry_lo,
      curve_point_num <= expiry_hi
    )

  if (nrow(forwards_df) == 0L) {
    return(ea_greeks_fallback(markets, labels))
  }

  vol_surface <- ea_generate_vol_surface(markets, forwards_df, catalog)

  today <- as.Date(latest_date)
  if (!is.null(expiry_meta) && nrow(expiry_meta) > 0L) {
    expiry_lookup <- expiry_meta |>
      dplyr::filter(market %in% markets) |>
      dplyr::group_by(market) |>
      dplyr::arrange(contract_year, contract_month) |>
      dplyr::mutate(cp_num = dplyr::row_number()) |>
      dplyr::ungroup() |>
      dplyr::select(market, cp_num, last_trade)

    vol_surface <- vol_surface |>
      dplyr::left_join(expiry_lookup, by = c("market" = "market", "curve_point_num" = "cp_num"))

    vol_surface$days_to_expiry <- ifelse(
      !is.na(vol_surface$last_trade),
      pmax(as.numeric(difftime(vol_surface$last_trade, today, units = "days")), 1),
      vol_surface$curve_point_num * 30
    )
  } else {
    vol_surface$days_to_expiry <- vol_surface$curve_point_num * 30
    vol_surface$last_trade <- NA
  }

  vol_surface$T_years <- vol_surface$days_to_expiry / 365.25
  vol_surface$risk_free <- ea_interpolate_risk_free(ust_curve, vol_surface$days_to_expiry)

  greeks_result <- black76_greeks_vectorised(
    F       = vol_surface$forward,
    K       = vol_surface$strike,
    r       = vol_surface$risk_free,
    T       = vol_surface$T_years,
    sigma   = vol_surface$iv,
    is_call = rep(TRUE, nrow(vol_surface))
  )

  full_grid <- dplyr::bind_cols(vol_surface, greeks_result)

  primary <- markets[1]

  cross_greeks_vanna <- full_grid |>
    dplyr::filter(market == primary) |>
    dplyr::select(curve_point_num, moneyness, vanna)

  cross_greeks_charm <- full_grid |>
    dplyr::filter(market == primary) |>
    dplyr::select(curve_point_num, moneyness, charm)

  term_greeks <- full_grid |>
    dplyr::filter(abs(moneyness - 1.0) < 0.01) |>
    dplyr::select(market, label, curve_point, curve_point_num, gamma, vega)

  prompt_num <- min(full_grid$curve_point_num)
  strike_profile <- full_grid |>
    dplyr::filter(market == primary, curve_point_num == prompt_num) |>
    dplyr::select(moneyness, strike, speed, zomma)

  primary_atm <- full_grid |>
    dplyr::filter(market == primary, abs(moneyness - 1.0) < 0.01)

  kpis <- tibble::tribble(
    ~title, ~value, ~delta, ~icon, ~status,
    "ATM Delta",
    if (nrow(primary_atm) > 0L) scales::number(mean(primary_atm$delta), accuracy = 0.001) else "N/A",
    "front contract", "chart-line", "neutral",

    "Peak Gamma",
    if (nrow(primary_atm) > 0L) scales::scientific(max(primary_atm$gamma), digits = 3) else "N/A",
    "across term structure", "gauge-high", "warning",

    "ATM Vega",
    if (nrow(primary_atm) > 0L) scales::number(mean(primary_atm$vega), accuracy = 0.01) else "N/A",
    "per 1 vol point", "wave-square", "accent",

    "Surface points",
    scales::comma(nrow(full_grid)),
    "computed via Black-76", "grid-4", "positive"
  )

  list(
    kpis = kpis,
    cross_greeks_vanna = cross_greeks_vanna,
    cross_greeks_charm = cross_greeks_charm,
    term_greeks = term_greeks,
    strike_profile = strike_profile,
    full_grid = full_grid,
    notes = c(
      "Greeks computed using the Black-76 futures options model via the Rcpp C++ engine.",
      "Implied vol surface is parameterised (sticky-delta skew with term decay) until real exchange vol data is available.",
      "All calculations run natively in C++ for maximum throughput across the grid."
    ),
    assumptions = c(
      "Risk-free rates interpolated linearly from the UST yield curve.",
      "Vol surface uses SABR-inspired sticky-delta parameterisation: sigma(K) = sigma_ATM * (1 + b1*log(K/F) + b2*log(K/F)^2).",
      "Days to expiry derived from contract_expiry_metadata last_trade dates where available."
    )
  )
}

ea_greeks_fallback <- function(markets, labels) {
  primary <- markets[1]
  tenors <- seq_len(12)
  moneyness_grid <- c(0.80, 0.85, 0.90, 0.95, 1.00, 1.05, 1.10, 1.15, 1.20)

  base_price <- ea_market_base_price(primary)
  base_vol <- ea_market_base_vol(primary)

  fallback_grid <- expand.grid(
    curve_point_num = tenors,
    moneyness = moneyness_grid,
    stringsAsFactors = FALSE
  ) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      market = primary,
      label = ea_lookup_value(labels, primary),
      forward = base_price + curve_point_num * ea_market_curve_slope(primary),
      strike = forward * moneyness,
      iv = base_vol * (curve_point_num / 12)^(-0.05) * (1 - 0.15 * log(moneyness) + 0.80 * log(moneyness)^2),
      T_years = curve_point_num * 30 / 365.25,
      risk_free = 0.045,
      curve_point = stringr::str_pad(as.character(curve_point_num), width = 2, pad = "0")
    )

  greeks_result <- black76_greeks_vectorised(
    F       = fallback_grid$forward,
    K       = fallback_grid$strike,
    r       = fallback_grid$risk_free,
    T       = fallback_grid$T_years,
    sigma   = fallback_grid$iv,
    is_call = rep(TRUE, nrow(fallback_grid))
  )

  full_grid <- dplyr::bind_cols(fallback_grid, greeks_result)

  cross_greeks_vanna <- full_grid |>
    dplyr::select(curve_point_num, moneyness, vanna)

  cross_greeks_charm <- full_grid |>
    dplyr::select(curve_point_num, moneyness, charm)

  term_greeks <- purrr::map_dfr(markets, function(mkt) {
    full_grid |>
      dplyr::filter(abs(moneyness - 1.0) < 0.01) |>
      dplyr::mutate(market = mkt, label = ea_lookup_value(labels, mkt)) |>
      dplyr::select(market, label, curve_point, curve_point_num, gamma, vega)
  })

  strike_profile <- full_grid |>
    dplyr::filter(curve_point_num == min(curve_point_num)) |>
    dplyr::select(moneyness, strike, speed, zomma)

  primary_atm <- full_grid |>
    dplyr::filter(abs(moneyness - 1.0) < 0.01)

  kpis <- tibble::tribble(
    ~title, ~value, ~delta, ~icon, ~status,
    "ATM Delta",
    if (nrow(primary_atm) > 0L) scales::number(mean(primary_atm$delta), accuracy = 0.001) else "N/A",
    "front contract", "chart-line", "neutral",
    "Peak Gamma",
    if (nrow(primary_atm) > 0L) scales::scientific(max(primary_atm$gamma), digits = 3) else "N/A",
    "across term structure", "gauge-high", "warning",
    "ATM Vega",
    if (nrow(primary_atm) > 0L) scales::number(mean(primary_atm$vega), accuracy = 0.01) else "N/A",
    "per 1 vol point", "wave-square", "accent",
    "Surface points",
    scales::comma(nrow(full_grid)),
    "computed via Black-76 (fallback)", "grid-4", "positive"
  )

  list(
    kpis = kpis,
    cross_greeks_vanna = cross_greeks_vanna,
    cross_greeks_charm = cross_greeks_charm,
    term_greeks = term_greeks,
    strike_profile = strike_profile,
    full_grid = full_grid,
    notes = c(
      "Greeks computed using Black-76 with fallback parameterised data (snapshot not available).",
      "Connect to the live data snapshot for production-grade outputs."
    ),
    assumptions = c(
      "Forwards, vols, and rates are synthetic fallback values.",
      "Results are structurally correct but not market-calibrated."
    )
  )
}
