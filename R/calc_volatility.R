# ---- Volatility Calculation Layer ----
# Realized vol, GARCH, vol surface, and implied vs realized analytics.

ea_calc_volatility <- function(filters) {
  catalog <- ea_market_catalog()
  curves <- ea_load_filtered_curves(filters)
  markets <- unique(curves$market)
  primary <- markets[1]
  labels <- stats::setNames(catalog$label, catalog$market)

  # Front contract returns for realized vol
  front <- curves |>
    dplyr::filter(.data$curve_point_num == 1) |>
    dplyr::arrange(.data$market, .data$date) |>
    dplyr::group_by(.data$market) |>
    dplyr::mutate(log_return = log(.data$value / dplyr::lag(.data$value))) |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(.data$log_return))

  # --- realized_vol_timeseries: rolling vol at 20d, 60d, 120d ---
  windows <- c("20d" = 20L, "60d" = 60L, "120d" = 120L)
  annualize <- sqrt(252)

  realized_vol_timeseries <- purrr::map_dfr(names(windows), function(w) {
    n <- windows[[w]]
    front |>
      dplyr::group_by(.data$market) |>
      dplyr::mutate(
        realized_vol = slider::slide_dbl(
          .data$log_return, stats::sd, .before = n - 1L, .complete = TRUE
        ) * annualize
      ) |>
      dplyr::ungroup() |>
      dplyr::filter(!is.na(.data$realized_vol)) |>
      dplyr::transmute(
        date = .data$date, market = .data$market,
        window = w, realized_vol = .data$realized_vol
      )
  })

  # --- garch_vol: GARCH(1,1) for primary market ---
  primary_front <- front |>
    dplyr::filter(.data$market == primary) |>
    dplyr::select(.data$date, value = .data$log_return)

  garch_vol <- tryCatch({
    garch_xts <- RTL::garch(x = primary_front, out = "data")
    tibble::tibble(
      date = as.Date(zoo::index(garch_xts)),
      returns = as.numeric(zoo::coredata(garch_xts[, "returns"])),
      garch_vol = as.numeric(zoo::coredata(garch_xts[, "garch"]))
    )
  }, error = function(e) {
    tibble::tibble(date = as.Date(character()), returns = numeric(), garch_vol = numeric())
  })

  # --- vol_term_structure: realized vol by tenor ---
  vol_term_structure <- curves |>
    dplyr::arrange(.data$market, .data$curve_point_num, .data$date) |>
    dplyr::group_by(.data$market, .data$curve_point_num) |>
    dplyr::mutate(log_return = log(.data$value / dplyr::lag(.data$value))) |>
    dplyr::filter(!is.na(.data$log_return)) |>
    dplyr::summarise(
      realized_vol = stats::sd(.data$log_return, na.rm = TRUE) * annualize,
      .groups = "drop"
    ) |>
    dplyr::rename(tenor = .data$curve_point_num)

  # --- vol_surface_grid: parametric IV surface ---
  latest_date <- max(curves$date, na.rm = TRUE)
  forwards_df <- curves |> dplyr::filter(.data$date == latest_date)

  vol_surface_grid <- ea_generate_vol_surface(markets, forwards_df, catalog)

  # --- vol_skew_snapshot: front tenor IV smile ---
  front_tenor <- min(vol_surface_grid$curve_point_num, na.rm = TRUE)
  vol_skew_snapshot <- vol_surface_grid |>
    dplyr::filter(.data$curve_point_num == front_tenor) |>
    dplyr::select(.data$market, .data$moneyness, .data$iv)

  # --- realized_vs_implied: 60d realized vs ATM implied ---
  rv_60 <- realized_vol_timeseries |>
    dplyr::filter(.data$window == "60d") |>
    dplyr::select(.data$date, .data$market, realized = .data$realized_vol)

  atm_iv <- vol_surface_grid |>
    dplyr::filter(abs(.data$moneyness - 1.0) < 0.01, .data$curve_point_num == front_tenor) |>
    dplyr::select(.data$market, implied = .data$iv)

  realized_vs_implied <- rv_60 |>
    dplyr::left_join(atm_iv, by = "market")

  # --- vol_cone: percentile bands by horizon ---
  vol_cone <- purrr::map_dfr(markets, function(mkt) {
    mkt_front <- front |> dplyr::filter(.data$market == mkt)
    if (nrow(mkt_front) < 252) return(tibble::tibble())

    horizons <- c(5L, 10L, 20L, 60L, 120L, 252L)
    purrr::map_dfr(horizons, function(h) {
      roll_vol <- slider::slide_dbl(
        mkt_front$log_return, stats::sd, .before = h - 1L, .complete = TRUE
      ) * annualize
      roll_vol <- roll_vol[!is.na(roll_vol)]
      if (length(roll_vol) == 0) return(tibble::tibble())

      current <- roll_vol[length(roll_vol)]
      tibble::tibble(
        market = mkt, horizon = h,
        current_vol = current,
        p10 = stats::quantile(roll_vol, 0.10, na.rm = TRUE),
        p25 = stats::quantile(roll_vol, 0.25, na.rm = TRUE),
        p50 = stats::quantile(roll_vol, 0.50, na.rm = TRUE),
        p75 = stats::quantile(roll_vol, 0.75, na.rm = TRUE),
        p90 = stats::quantile(roll_vol, 0.90, na.rm = TRUE)
      )
    })
  })

  # --- vol_of_vol: rolling SD of 20d RV ---
  vol_of_vol <- purrr::map_dfr(markets, function(mkt) {
    rv_20 <- realized_vol_timeseries |>
      dplyr::filter(.data$market == mkt, .data$window == "20d")
    if (nrow(rv_20) < 60) return(tibble::tibble())

    rv_20 |>
      dplyr::mutate(
        vol_of_vol = slider::slide_dbl(
          .data$realized_vol, stats::sd, .before = 59L, .complete = TRUE
        )
      ) |>
      dplyr::filter(!is.na(.data$vol_of_vol)) |>
      dplyr::select("date", "market", "vol_of_vol")
  })

  # --- cross_asset_vol: distribution data per commodity ---
  cross_asset_vol <- purrr::map_dfr(markets, function(mkt) {
    rv_20 <- realized_vol_timeseries |>
      dplyr::filter(.data$market == mkt, .data$window == "20d")
    if (nrow(rv_20) == 0) return(tibble::tibble())

    current <- rv_20$realized_vol[nrow(rv_20)]
    tibble::tibble(
      market = mkt,
      current_vol = current,
      min_vol = min(rv_20$realized_vol, na.rm = TRUE),
      q1 = stats::quantile(rv_20$realized_vol, 0.25, na.rm = TRUE),
      median_vol = stats::median(rv_20$realized_vol, na.rm = TRUE),
      q3 = stats::quantile(rv_20$realized_vol, 0.75, na.rm = TRUE),
      max_vol = max(rv_20$realized_vol, na.rm = TRUE),
      percentile = mean(rv_20$realized_vol <= current, na.rm = TRUE)
    )
  })

  # --- vol_regime: classify by percentile + trend ---
  vol_regime <- purrr::map_dfr(markets, function(mkt) {
    rv_20 <- realized_vol_timeseries |>
      dplyr::filter(.data$market == mkt, .data$window == "20d")
    if (nrow(rv_20) < 30) return(tibble::tibble(market = mkt, regime = "Insufficient Data", vol_percentile = NA_real_))

    current <- rv_20$realized_vol[nrow(rv_20)]
    pct <- mean(rv_20$realized_vol <= current, na.rm = TRUE)
    recent_trend <- mean(utils::tail(rv_20$realized_vol, 10), na.rm = TRUE) - mean(utils::tail(rv_20$realized_vol, 30), na.rm = TRUE)

    level <- if (pct > 0.75) "Elevated" else if (pct < 0.25) "Low" else "Normal"
    direction <- if (recent_trend > 0.01) "Rising" else if (recent_trend < -0.01) "Falling" else "Stable"

    tibble::tibble(market = mkt, regime = paste(level, "&", direction), vol_percentile = pct)
  })

  # --- iv_rv_spread: ATM IV minus 60d realized vol time series ---
  atm_iv_value <- vol_surface_grid |>
    dplyr::filter(abs(.data$moneyness - 1.0) < 0.01, .data$curve_point_num == front_tenor) |>
    dplyr::group_by(.data$market) |>
    dplyr::summarise(atm_iv = mean(.data$iv, na.rm = TRUE), .groups = "drop")

  iv_rv_spread <- realized_vol_timeseries |>
    dplyr::filter(.data$window == "60d") |>
    dplyr::select("date", "market", realized = "realized_vol") |>
    dplyr::left_join(atm_iv_value, by = "market") |>
    dplyr::mutate(iv_rv_spread = .data$atm_iv - .data$realized)

  # --- KPIs ---
  primary_rv20 <- realized_vol_timeseries |> dplyr::filter(.data$market == primary, .data$window == "20d")
  primary_rv60 <- realized_vol_timeseries |> dplyr::filter(.data$market == primary, .data$window == "60d")
  primary_regime <- vol_regime |> dplyr::filter(.data$market == primary)
  primary_iv <- atm_iv_value |> dplyr::filter(.data$market == primary)

  rv20_current <- if (nrow(primary_rv20) > 0) primary_rv20$realized_vol[nrow(primary_rv20)] else NA_real_
  rv60_current <- if (nrow(primary_rv60) > 0) primary_rv60$realized_vol[nrow(primary_rv60)] else NA_real_
  iv_current <- if (nrow(primary_iv) > 0) primary_iv$atm_iv[1] else NA_real_
  iv_rv_diff <- if (!is.na(iv_current) && !is.na(rv60_current)) iv_current - rv60_current else NA_real_

  kpis <- tibble::tribble(
    ~title, ~value, ~delta, ~status,
    "20d Realized",
    if (!is.na(rv20_current)) scales::percent(rv20_current, accuracy = 0.1) else "N/A",
    "annualized", "neutral",
    "60d Realized",
    if (!is.na(rv60_current)) scales::percent(rv60_current, accuracy = 0.1) else "N/A",
    "annualized", "neutral",
    "ATM Implied",
    if (!is.na(iv_current)) scales::percent(iv_current, accuracy = 0.1) else "N/A",
    "front month", "neutral",
    "IV-RV Spread",
    if (!is.na(iv_rv_diff)) paste0(ifelse(iv_rv_diff >= 0, "+", ""), scales::percent(iv_rv_diff, accuracy = 0.1)) else "N/A",
    if (!is.na(iv_rv_diff) && iv_rv_diff > 0) "options rich" else "options cheap",
    if (!is.na(iv_rv_diff) && iv_rv_diff > 0) "warning" else "positive",
    "Vol Percentile",
    if (nrow(primary_regime) > 0 && !is.na(primary_regime$vol_percentile[1])) scales::percent(primary_regime$vol_percentile[1], accuracy = 1) else "N/A",
    "vs 1Y history", "neutral",
    "Vol Regime",
    if (nrow(primary_regime) > 0) primary_regime$regime[1] else "N/A",
    labels[primary], if (nrow(primary_regime) > 0 && grepl("Elevated", primary_regime$regime[1])) "warning" else "positive"
  )

  notes <- c(
    "Realized volatility computed from log returns, annualized by sqrt(252).",
    "GARCH(1,1) conditional vol computed via RTL::garch on front-month returns.",
    "Vol surface is parameterised (sticky-delta skew) until exchange IV data is available."
  )
  assumptions <- c(
    "Vol cone percentiles computed over the full available history.",
    "Vol-of-vol uses a 60-day rolling window on the 20d realized vol series."
  )

  list(
    realized_vol_timeseries = realized_vol_timeseries,
    garch_vol = garch_vol,
    vol_term_structure = vol_term_structure,
    vol_surface_grid = vol_surface_grid,
    vol_skew_snapshot = vol_skew_snapshot,
    realized_vs_implied = realized_vs_implied,
    vol_cone = vol_cone,
    vol_of_vol = vol_of_vol,
    cross_asset_vol = cross_asset_vol,
    vol_regime = vol_regime,
    iv_rv_spread = iv_rv_spread,
    kpis = kpis,
    notes = notes,
    assumptions = assumptions
  )
}
