# ---- Options Greeks Calculation Pipeline ----
# Bridges canonical Feather datasets to the Rcpp Black-76 engine.

#' @useDynLib EnergyAnalytics, .registration = TRUE
#' @importFrom Rcpp sourceCpp
NULL

ea_interpolate_risk_free <- function(ust_curve, days_to_expiry, target_date = NULL) {
  target_date <- target_date %||% max(ust_curve$date, na.rm = TRUE)

  ust_snapshot <- ust_curve |>
    dplyr::filter(.data$date == as.Date(target_date)) |>
    dplyr::arrange(.data$curve_point_num) |>
    dplyr::select(.data$curve_point_num, .data$value)

  if (nrow(ust_snapshot) == 0L) {
    # Fallback to latest if date not found
    ust_snapshot <- ust_curve |>
      dplyr::filter(.data$date == max(.data$date)) |>
      dplyr::arrange(.data$curve_point_num) |>
      dplyr::select(.data$curve_point_num, .data$value)
  }

  tenors_years <- ust_snapshot$curve_point_num
  rates <- ust_snapshot$value / 100

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

#' Build the Unified Options Cube
#' @export
ea_build_options_surface_long <- function(raw_options_dir = "data-raw/options_chains",
                                          futures = NULL,
                                          ust_curve = NULL,
                                          expiry_meta = NULL) {
  # Schema definition for consistency in empty returns
  empty_result <- tibble::tibble(
    date = as.Date(character()),
    market = character(),
    contract_month = character(),
    days_to_expiry = numeric(),
    strike = numeric(),
    option_type = character(),
    underlying_price = numeric(),
    implied_volatility = numeric(),
    delta = numeric(),
    gamma = numeric(),
    vega = numeric(),
    theta = numeric(),
    rho = numeric(),
    vanna = numeric(),
    charm = numeric(),
    speed = numeric(),
    zomma = numeric(),
    color = numeric(),
    ultima = numeric()
  )

  # 1. Load Input Data
  options_files <- list.files(raw_options_dir, pattern = "\\.csv$", full.names = TRUE)
  if (length(options_files) == 0L) {
    return(empty_result)
  }

  raw_options <- purrr::map_dfr(options_files, readr::read_csv, show_col_types = FALSE) |>
    dplyr::mutate(obs_date = as.Date(.data$obs_date))

  # Use provided dependencies or load from disk
  futures <- futures %||% ea_load_dataset("commodity_curve_long")
  ust_curve <- ust_curve %||% ea_load_dataset("ust_curve_long")
  expiry_meta <- expiry_meta %||% ea_load_dataset("contract_expiry_metadata")

  # 2. Map Futures Curve Points to Contract Months (Optimized)
  target_dates <- unique(raw_options$obs_date)
  target_markets <- unique(raw_options$market)

  # Create a mapping of (market, date, curve_point_num) -> contract_month
  # This uses the expiry table to find which contract is '01', '02', etc. on each date.
  curve_map <- expiry_meta |>
    dplyr::filter(.data$market %in% target_markets) |>
    dplyr::select(.data$market, .data$contract_year, .data$contract_month, .data$last_trade) |>
    dplyr::cross_join(tibble::tibble(obs_date = target_dates)) |>
    dplyr::filter(.data$last_trade >= .data$obs_date) |>
    dplyr::group_by(.data$market, .data$obs_date) |>
    dplyr::arrange(.data$last_trade) |>
    dplyr::mutate(curve_point_num = as.numeric(dplyr::row_number())) |>
    dplyr::ungroup() |>
    dplyr::mutate(contract_month_label = sprintf("%04d-%02d", .data$contract_year, .data$contract_month)) |>
    dplyr::select(.data$obs_date, .data$market, .data$curve_point_num, contract_month = .data$contract_month_label, .data$last_trade)

  # Join curve_map to futures to get the underlying price for each contract month
  futures_mapped <- futures |>
    dplyr::filter(.data$date %in% target_dates, .data$market %in% target_markets) |>
    dplyr::inner_join(curve_map, by = c("date" = "obs_date", "market", "curve_point_num")) |>
    dplyr::select(.data$date, .data$market, .data$contract_month, underlying_price = .data$value, .data$last_trade)

  # 3. Join Raw Options to Mapped Futures
  options_prep <- raw_options |>
    dplyr::inner_join(futures_mapped, by = c("obs_date" = "date", "market", "contract_month")) |>
    dplyr::mutate(
      dte = pmax(as.numeric(difftime(.data$last_trade, .data$obs_date, units = "days")), 1),
      T_years = .data$dte / 365.25
    )

  if (nrow(options_prep) == 0L) return(empty_result)

  # 4. Calculate Risk Free Rates
  options_prep$risk_free <- purrr::map2_dbl(
    options_prep$dte, options_prep$obs_date,
    ~ ea_interpolate_risk_free(ust_curve, .x, .y)
  )

  # 4. Calculate IV (Newton-Raphson)
  calc_iv <- function(F, K, r, T, premium, is_call) {
    sig <- rep(0.3, length(F))
    for (i in 1:15) {
      g <- black76_greeks_vectorised(F, K, r, T, sig, is_call)
      diff <- g$premium - premium
      idx <- !is.na(diff) & abs(diff) > 1e-6
      if (!any(idx)) break
      update_idx <- idx & !is.na(g$vega) & g$vega > 1e-8
      if (!any(update_idx)) break
      sig[update_idx] <- sig[update_idx] - (diff[update_idx] / g$vega[update_idx])
      sig <- pmax(pmin(sig, 5.0), 0.001)
    }
    sig
  }

  options_prep$iv <- calc_iv(
    options_prep$underlying_price, options_prep$strike, options_prep$risk_free,
    options_prep$T_years, options_prep$premium, options_prep$type == "C"
  )

  # 5. Calculate All Greeks
  greeks <- black76_greeks_vectorised(
    options_prep$underlying_price, options_prep$strike, options_prep$risk_free,
    options_prep$T_years, options_prep$iv, options_prep$type == "C"
  )

  # 6. Final Formatting
  options_prep |>
    dplyr::mutate(
      implied_volatility = .data$iv,
      delta = greeks$delta,
      gamma = greeks$gamma,
      vega = greeks$vega,
      theta = greeks$theta,
      rho = greeks$rho,
      vanna = greeks$vanna,
      charm = greeks$charm,
      speed = greeks$speed,
      zomma = greeks$zomma,
      color = greeks$color,
      ultima = greeks$ultima
    ) |>
    dplyr::select(
      date = .data$obs_date,
      .data$market,
      .data$contract_month,
      days_to_expiry = .data$dte,
      .data$strike,
      option_type = .data$type,
      .data$underlying_price,
      .data$implied_volatility,
      .data$delta,
      .data$gamma,
      .data$vega,
      .data$theta,
      .data$rho,
      .data$vanna,
      .data$charm,
      .data$speed,
      .data$zomma,
      .data$color,
      .data$ultima
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
    dplyr::filter(abs(.data$moneyness - 1.0) < 0.01) |>
    dplyr::select(.data$market, .data$label, .data$curve_point, .data$curve_point_num, .data$gamma, .data$vega)

  prompt_num <- min(full_grid$curve_point_num)
  strike_profile <- full_grid |>
    dplyr::filter(.data$market == primary, .data$curve_point_num == prompt_num) |>
    dplyr::select(.data$moneyness, .data$strike, .data$speed, .data$zomma)

  primary_atm <- full_grid |>
    dplyr::filter(.data$market == primary, abs(.data$moneyness - 1.0) < 0.01)

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
      dplyr::filter(abs(.data$moneyness - 1.0) < 0.01) |>
      dplyr::mutate(market = mkt, label = ea_lookup_value(labels, mkt)) |>
      dplyr::select(.data$market, .data$label, .data$curve_point, .data$curve_point_num, .data$gamma, .data$vega)
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
