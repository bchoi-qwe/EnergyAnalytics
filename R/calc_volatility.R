# ---- Volatility Calculation Layer ----
# Realized vol, regime context, cone analytics, and implied-vs-realized structure.

ea_build_vol_returns <- function(curves, group_vars) {
  group_syms <- rlang::syms(group_vars)

  curves %>%
    dplyr::arrange(!!!group_syms, .data$date) %>%
    dplyr::group_by(!!!group_syms) %>%
    dplyr::mutate(
      prev_value = dplyr::lag(.data$value),
      raw_log_return = suppressWarnings(log(.data$value / .data$prev_value)),
      log_return = dplyr::if_else(
        is.finite(.data$value) & is.finite(.data$prev_value) & .data$value > 0 & .data$prev_value > 0,
        .data$raw_log_return,
        NA_real_
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(.data$log_return))
}

ea_build_realized_vol_timeseries <- function(return_data, windows, annualize = sqrt(252), group_cols = c("market")) {
  if (nrow(return_data) == 0L) {
    return(tibble::tibble(
      date = as.Date(character()),
      market = character(),
      window = character(),
      realized_vol = numeric()
    ))
  }

  group_syms <- rlang::syms(group_cols)

  purrr::map_dfr(names(windows), function(window_label) {
    lookback <- windows[[window_label]]

    return_data %>%
      dplyr::group_by(!!!group_syms) %>%
      dplyr::mutate(
        realized_vol = slider::slide_dbl(
          .data$log_return,
          stats::sd,
          .before = lookback - 1L,
          .complete = TRUE
        ) * annualize
      ) %>%
      dplyr::ungroup() %>%
      dplyr::filter(is.finite(.data$realized_vol)) %>%
      dplyr::transmute(
        date = .data$date,
        market = .data$market,
        window = window_label,
        realized_vol = .data$realized_vol
      )
  })
}

ea_build_vol_distribution <- function(vol_timeseries, window = "20d") {
  current_data <- vol_timeseries %>%
    dplyr::filter(.data$window == window) %>%
    dplyr::arrange(.data$market, .data$date)

  if (nrow(current_data) == 0L) {
    return(tibble::tibble(
      market = character(),
      current_vol = numeric(),
      min_vol = numeric(),
      p10 = numeric(),
      q1 = numeric(),
      median_vol = numeric(),
      q3 = numeric(),
      p90 = numeric(),
      max_vol = numeric(),
      percentile = numeric(),
      zscore = numeric()
    ))
  }

  current_data %>%
    dplyr::group_by(.data$market) %>%
    dplyr::summarise(
      current_vol = dplyr::last(.data$realized_vol),
      min_vol = min(.data$realized_vol, na.rm = TRUE),
      p10 = stats::quantile(.data$realized_vol, 0.10, na.rm = TRUE, names = FALSE),
      q1 = stats::quantile(.data$realized_vol, 0.25, na.rm = TRUE, names = FALSE),
      median_vol = stats::median(.data$realized_vol, na.rm = TRUE),
      q3 = stats::quantile(.data$realized_vol, 0.75, na.rm = TRUE, names = FALSE),
      p90 = stats::quantile(.data$realized_vol, 0.90, na.rm = TRUE, names = FALSE),
      max_vol = max(.data$realized_vol, na.rm = TRUE),
      mean_vol = mean(.data$realized_vol, na.rm = TRUE),
      sd_vol = stats::sd(.data$realized_vol, na.rm = TRUE),
      percentile = mean(.data$realized_vol <= dplyr::last(.data$realized_vol), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      zscore = dplyr::if_else(
        is.finite(.data$sd_vol) & .data$sd_vol > 0,
        (.data$current_vol - .data$mean_vol) / .data$sd_vol,
        NA_real_
      )
    ) %>%
    dplyr::select(
      "market", "current_vol", "min_vol", "p10", "q1", "median_vol",
      "q3", "p90", "max_vol", "percentile", "zscore"
    )
}

ea_latest_realized_vol <- function(x, lookback, annualize = sqrt(252)) {
  roll_vol <- slider::slide_dbl(
    x,
    stats::sd,
    .before = lookback - 1L,
    .complete = TRUE
  ) * annualize

  roll_vol <- roll_vol[is.finite(roll_vol)]
  if (length(roll_vol) == 0L) {
    NA_real_
  } else {
    utils::tail(roll_vol, 1L)
  }
}

ea_calc_volatility <- function(filters, history_context = "5y") {
  catalog <- ea_market_catalog()
  history_context <- ea_normalize_history_context(history_context)
  display_curves <- ea_load_filtered_curves(filters, apply_date_range = TRUE)
  all_curves <- ea_load_filtered_curves(filters, apply_date_range = FALSE)
  selected_markets <- ea_coalesce(filters$commodities, catalog$market)
  annualize <- sqrt(252)
  windows <- c("20d" = 20L, "60d" = 60L, "120d" = 120L)

  empty_realized <- tibble::tibble(
    date = as.Date(character()),
    market = character(),
    window = character(),
    realized_vol = numeric()
  )
  empty_term <- tibble::tibble(
    market = character(),
    tenor = numeric(),
    rv20 = numeric(),
    rv60 = numeric(),
    rv120 = numeric(),
    realized_vol = numeric()
  )
  empty_options_grid <- tibble::tibble(
    market = character(),
    label = character(),
    curve_point = character(),
    curve_point_num = numeric(),
    forward = numeric(),
    strike = numeric(),
    moneyness = numeric(),
    iv = numeric(),
    snapshot_date = as.Date(character())
  )
  empty_iv_rv <- tibble::tibble(
    date = as.Date(character()),
    market = character(),
    realized = numeric(),
    rv20 = numeric(),
    rv60 = numeric(),
    atm_iv = numeric(),
    iv_rv_spread = numeric()
  )
  empty_cone <- tibble::tibble(
    market = character(),
    horizon = numeric(),
    current_vol = numeric(),
    percentile = numeric(),
    zscore = numeric(),
    p10 = numeric(),
    p25 = numeric(),
    p50 = numeric(),
    p75 = numeric(),
    p90 = numeric()
  )
  empty_cross <- tibble::tibble(
    market = character(),
    current_vol = numeric(),
    min_vol = numeric(),
    p10 = numeric(),
    q1 = numeric(),
    median_vol = numeric(),
    q3 = numeric(),
    p90 = numeric(),
    max_vol = numeric(),
    percentile = numeric(),
    zscore = numeric()
  )
  empty_regime <- tibble::tibble(
    market = character(),
    regime = character(),
    vol_percentile = numeric(),
    vol_zscore = numeric()
  )
  empty_vov_context <- tibble::tibble(
    market = character(),
    current_vov = numeric(),
    p25 = numeric(),
    p50 = numeric(),
    p75 = numeric(),
    percentile = numeric()
  )

  if (nrow(display_curves) == 0L) {
    return(list(
      available_markets = character(),
      focus_market = NA_character_,
      history_context = history_context,
      history_context_label = ea_history_context_label(history_context),
      display_window = as.Date(c(NA, NA)),
      context_window = as.Date(c(NA, NA)),
      latest_options_date = as.Date(NA),
      realized_vol_timeseries = empty_realized,
      garch_vol = tibble::tibble(date = as.Date(character()), returns = numeric(), garch_vol = numeric()),
      vol_term_structure = empty_term,
      vol_surface_grid = empty_options_grid,
      vol_skew_snapshot = tibble::tibble(market = character(), moneyness = numeric(), iv = numeric()),
      realized_vs_implied = empty_iv_rv,
      vol_cone = empty_cone,
      vol_of_vol = tibble::tibble(date = as.Date(character()), market = character(), vol_of_vol = numeric()),
      vol_of_vol_context = empty_vov_context,
      vol_history_context = tibble::tibble(
        market = character(),
        window = character(),
        current_vol = numeric(),
        p10 = numeric(),
        p25 = numeric(),
        p50 = numeric(),
        p75 = numeric(),
        p90 = numeric(),
        percentile = numeric(),
        zscore = numeric()
      ),
      cross_asset_vol = empty_cross,
      vol_regime = empty_regime,
      iv_rv_spread = empty_iv_rv,
      kpis = tibble::tibble(title = character(), value = character(), delta = character(), status = character()),
      notes = c("Volatility views use the selected display window."),
      assumptions = c("History context is empty because no display-window data is available.")
    ))
  }

  available_markets <- selected_markets[selected_markets %in% unique(display_curves$market)]
  if (length(available_markets) == 0L) {
    available_markets <- unique(display_curves$market)
  }

  display_curves <- display_curves %>%
    dplyr::filter(.data$market %in% available_markets) %>%
    dplyr::mutate(market_order = match(.data$market, available_markets)) %>%
    dplyr::arrange(.data$market_order, .data$curve_point_num, .data$date) %>%
    dplyr::select(-"market_order")

  all_curves <- all_curves %>%
    dplyr::filter(.data$market %in% available_markets) %>%
    dplyr::mutate(market_order = match(.data$market, available_markets)) %>%
    dplyr::arrange(.data$market_order, .data$curve_point_num, .data$date) %>%
    dplyr::select(-"market_order")

  primary <- available_markets[[1]]
  display_window <- as.Date(c(min(display_curves$date, na.rm = TRUE), max(display_curves$date, na.rm = TRUE)))
  context_start_date <- ea_history_context_start_date(
    end_date = display_window[[2]],
    history_context = history_context,
    min_date = min(all_curves$date, na.rm = TRUE)
  )
  context_curves <- all_curves %>%
    dplyr::filter(.data$date >= context_start_date, .data$date <= display_window[[2]])

  if (nrow(context_curves) == 0L) {
    context_curves <- display_curves
    context_start_date <- min(context_curves$date, na.rm = TRUE)
  }

  context_window <- as.Date(c(context_start_date, max(context_curves$date, na.rm = TRUE)))

  front_display <- display_curves %>%
    dplyr::filter(.data$curve_point_num == min(.data$curve_point_num, na.rm = TRUE)) %>%
    ea_build_vol_returns(group_vars = c("market"))

  front_context <- context_curves %>%
    dplyr::filter(.data$curve_point_num == min(.data$curve_point_num, na.rm = TRUE)) %>%
    ea_build_vol_returns(group_vars = c("market"))

  realized_vol_timeseries <- ea_build_realized_vol_timeseries(
    return_data = front_display,
    windows = windows,
    annualize = annualize
  )

  realized_vol_context <- ea_build_realized_vol_timeseries(
    return_data = front_context,
    windows = windows,
    annualize = annualize
  )

  vol_history_context <- realized_vol_context %>%
    dplyr::group_by(.data$market, .data$window) %>%
    dplyr::summarise(
      current_vol = dplyr::last(.data$realized_vol),
      mean_vol = mean(.data$realized_vol, na.rm = TRUE),
      sd_vol = stats::sd(.data$realized_vol, na.rm = TRUE),
      p10 = stats::quantile(.data$realized_vol, 0.10, na.rm = TRUE, names = FALSE),
      p25 = stats::quantile(.data$realized_vol, 0.25, na.rm = TRUE, names = FALSE),
      p50 = stats::quantile(.data$realized_vol, 0.50, na.rm = TRUE, names = FALSE),
      p75 = stats::quantile(.data$realized_vol, 0.75, na.rm = TRUE, names = FALSE),
      p90 = stats::quantile(.data$realized_vol, 0.90, na.rm = TRUE, names = FALSE),
      percentile = mean(.data$realized_vol <= dplyr::last(.data$realized_vol), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      zscore = dplyr::if_else(
        is.finite(.data$sd_vol) & .data$sd_vol > 0,
        (.data$current_vol - .data$mean_vol) / .data$sd_vol,
        NA_real_
      )
    ) %>%
    dplyr::select(
      "market", "window", "current_vol", "p10", "p25",
      "p50", "p75", "p90", "percentile", "zscore"
    )

  primary_front <- front_display %>%
    dplyr::filter(.data$market == primary) %>%
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

  term_returns <- all_curves %>%
    dplyr::filter(.data$date <= display_window[[2]]) %>%
    ea_build_vol_returns(group_vars = c("market", "curve_point_num"))

  vol_term_structure <- term_returns %>%
    dplyr::group_by(.data$market, .data$curve_point_num) %>%
    dplyr::summarise(
      rv20 = ea_latest_realized_vol(.data$log_return, 20L, annualize = annualize),
      rv60 = ea_latest_realized_vol(.data$log_return, 60L, annualize = annualize),
      rv120 = ea_latest_realized_vol(.data$log_return, 120L, annualize = annualize),
      .groups = "drop"
    ) %>%
    dplyr::transmute(
      market = .data$market,
      tenor = .data$curve_point_num,
      rv20 = .data$rv20,
      rv60 = .data$rv60,
      rv120 = .data$rv120,
      realized_vol = .data$rv60
    )

  options_surface <- ea_load_dataset("options_surface_long") %>%
    dplyr::filter(.data$market %in% available_markets, .data$date <= display_window[[2]])

  latest_options_date <- as.Date(NA)

  if (nrow(options_surface) == 0L) {
    vol_surface_grid <- empty_options_grid
    vol_skew_snapshot <- tibble::tibble(market = character(), moneyness = numeric(), iv = numeric())
    atm_history <- tibble::tibble(date = as.Date(character()), market = character(), atm_iv = numeric())
    realized_vs_implied <- empty_iv_rv
  } else {
    if (!"curve_point_num" %in% names(options_surface)) {
      options_surface$curve_point_num <- NA_real_
    }

    options_surface <- options_surface %>%
      dplyr::mutate(
        curve_point_num = as.numeric(.data$curve_point_num),
        moneyness = .data$strike / .data$underlying_price,
        iv = .data$implied_volatility,
        forward = .data$underlying_price,
        curve_point = .data$contract_month
      )

    if (all(is.na(options_surface$curve_point_num))) {
      options_surface <- options_surface %>%
        dplyr::group_by(.data$date, .data$market) %>%
        dplyr::arrange(.data$days_to_expiry, .data$contract_month, .by_group = TRUE) %>%
        dplyr::mutate(curve_point_num = dplyr::dense_rank(.data$days_to_expiry)) %>%
        dplyr::ungroup()
    }

    options_display <- options_surface %>%
      dplyr::filter(
        .data$date >= display_window[[1]],
        .data$date <= display_window[[2]],
        !is.na(.data$curve_point_num),
        .data$curve_point_num >= ea_coalesce(filters$expiry_range[1], 1L),
        .data$curve_point_num <= ea_coalesce(filters$expiry_range[2], 12L)
      )

    if (nrow(options_display) > 0L) {
      latest_options_date <- max(options_display$date, na.rm = TRUE)
    } else {
      latest_options_date <- max(options_surface$date, na.rm = TRUE)
    }

    vol_surface_grid <- options_surface %>%
      dplyr::filter(
        !is.na(.data$curve_point_num),
        .data$curve_point_num >= ea_coalesce(filters$expiry_range[1], 1L),
        .data$curve_point_num <= ea_coalesce(filters$expiry_range[2], 12L),
        .data$date == latest_options_date
      ) %>%
      dplyr::transmute(
        .data$market,
        label = ea_market_labels(.data$market, catalog),
        .data$curve_point,
        .data$curve_point_num,
        .data$forward,
        .data$strike,
        .data$moneyness,
        iv = .data$iv,
        snapshot_date = .data$date
      )

    front_tenor <- if (nrow(vol_surface_grid) > 0L) min(vol_surface_grid$curve_point_num, na.rm = TRUE) else NA_real_
    vol_skew_snapshot <- vol_surface_grid %>%
      dplyr::filter(.data$curve_point_num == front_tenor) %>%
      dplyr::select(.data$market, .data$moneyness, iv = .data$iv)

    rv20_display <- realized_vol_timeseries %>%
      dplyr::filter(.data$window == "20d") %>%
      dplyr::select(.data$date, .data$market, rv20 = .data$realized_vol)

    rv60_display <- realized_vol_timeseries %>%
      dplyr::filter(.data$window == "60d") %>%
      dplyr::select(.data$date, .data$market, realized = .data$realized_vol, rv60 = .data$realized_vol)

    atm_history <- options_display %>%
      dplyr::filter(!is.na(.data$curve_point_num)) %>%
      dplyr::group_by(.data$date, .data$market) %>%
      dplyr::filter(.data$curve_point_num == min(.data$curve_point_num, na.rm = TRUE)) %>%
      dplyr::slice_min(abs(.data$moneyness - 1), n = 1L, with_ties = FALSE) %>%
      dplyr::ungroup() %>%
      dplyr::transmute(.data$date, .data$market, atm_iv = .data$iv)

    realized_vs_implied <- rv60_display %>%
      dplyr::left_join(rv20_display, by = c("date", "market")) %>%
      dplyr::left_join(atm_history, by = c("date", "market")) %>%
      dplyr::mutate(iv_rv_spread = .data$atm_iv - .data$realized)
  }

  vol_cone <- purrr::map_dfr(available_markets, function(mkt) {
    mkt_front <- front_context %>% dplyr::filter(.data$market == mkt)
    if (nrow(mkt_front) < 252L) {
      return(tibble::tibble())
    }

    horizons <- c(5L, 10L, 20L, 60L, 120L, 252L)
    purrr::map_dfr(horizons, function(horizon) {
      roll_vol <- slider::slide_dbl(
        mkt_front$log_return,
        stats::sd,
        .before = horizon - 1L,
        .complete = TRUE
      ) * annualize

      roll_vol <- roll_vol[is.finite(roll_vol)]
      if (length(roll_vol) == 0L) {
        return(tibble::tibble())
      }

      current_vol <- utils::tail(roll_vol, 1L)
      mean_vol <- mean(roll_vol, na.rm = TRUE)
      sd_vol <- stats::sd(roll_vol, na.rm = TRUE)

      tibble::tibble(
        market = mkt,
        horizon = horizon,
        current_vol = current_vol,
        percentile = mean(roll_vol <= current_vol, na.rm = TRUE),
        zscore = if (is.finite(sd_vol) && sd_vol > 0) (current_vol - mean_vol) / sd_vol else NA_real_,
        p10 = stats::quantile(roll_vol, 0.10, na.rm = TRUE, names = FALSE),
        p25 = stats::quantile(roll_vol, 0.25, na.rm = TRUE, names = FALSE),
        p50 = stats::quantile(roll_vol, 0.50, na.rm = TRUE, names = FALSE),
        p75 = stats::quantile(roll_vol, 0.75, na.rm = TRUE, names = FALSE),
        p90 = stats::quantile(roll_vol, 0.90, na.rm = TRUE, names = FALSE)
      )
    })
  })

  vol_of_vol <- realized_vol_timeseries %>%
    dplyr::filter(.data$window == "20d") %>%
    dplyr::group_by(.data$market) %>%
    dplyr::arrange(.data$date, .by_group = TRUE) %>%
    dplyr::mutate(
      vol_of_vol = slider::slide_dbl(
        .data$realized_vol,
        stats::sd,
        .before = 59L,
        .complete = TRUE
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(is.finite(.data$vol_of_vol)) %>%
    dplyr::select("date", "market", "vol_of_vol")

  vol_of_vol_context <- realized_vol_context %>%
    dplyr::filter(.data$window == "20d") %>%
    dplyr::group_by(.data$market) %>%
    dplyr::arrange(.data$date, .by_group = TRUE) %>%
    dplyr::mutate(
      vol_of_vol = slider::slide_dbl(
        .data$realized_vol,
        stats::sd,
        .before = 59L,
        .complete = TRUE
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(is.finite(.data$vol_of_vol)) %>%
    dplyr::group_by(.data$market) %>%
    dplyr::summarise(
      current_vov = dplyr::last(.data$vol_of_vol),
      p25 = stats::quantile(.data$vol_of_vol, 0.25, na.rm = TRUE, names = FALSE),
      p50 = stats::quantile(.data$vol_of_vol, 0.50, na.rm = TRUE, names = FALSE),
      p75 = stats::quantile(.data$vol_of_vol, 0.75, na.rm = TRUE, names = FALSE),
      percentile = mean(.data$vol_of_vol <= dplyr::last(.data$vol_of_vol), na.rm = TRUE),
      .groups = "drop"
    )

  cross_asset_vol <- ea_build_vol_distribution(realized_vol_context, window = "20d")

  vol_regime <- realized_vol_context %>%
    dplyr::filter(.data$window == "20d") %>%
    dplyr::group_by(.data$market) %>%
    dplyr::arrange(.data$date, .by_group = TRUE) %>%
    dplyr::summarise(
      current_vol = dplyr::last(.data$realized_vol),
      vol_percentile = mean(.data$realized_vol <= dplyr::last(.data$realized_vol), na.rm = TRUE),
      mean_vol = mean(.data$realized_vol, na.rm = TRUE),
      sd_vol = stats::sd(.data$realized_vol, na.rm = TRUE),
      recent_trend = if (dplyr::n() >= 30L) {
        mean(utils::tail(.data$realized_vol, 10L), na.rm = TRUE) -
          mean(utils::tail(.data$realized_vol, 30L), na.rm = TRUE)
      } else {
        NA_real_
      },
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      level = dplyr::case_when(
        .data$vol_percentile >= 0.80 ~ "Elevated",
        .data$vol_percentile <= 0.20 ~ "Low",
        TRUE ~ "Normal"
      ),
      direction = dplyr::case_when(
        is.na(.data$recent_trend) ~ "Stable",
        .data$recent_trend > 0.01 ~ "Rising",
        .data$recent_trend < -0.01 ~ "Falling",
        TRUE ~ "Stable"
      ),
      regime = paste(.data$level, "&", .data$direction),
      vol_zscore = dplyr::if_else(
        is.finite(.data$sd_vol) & .data$sd_vol > 0,
        (.data$current_vol - .data$mean_vol) / .data$sd_vol,
        NA_real_
      )
    ) %>%
    dplyr::select("market", "regime", "vol_percentile", "vol_zscore")

  iv_rv_spread <- realized_vs_implied

  primary_rv20 <- vol_history_context %>%
    dplyr::filter(.data$market == primary, .data$window == "20d")
  primary_rv60 <- vol_history_context %>%
    dplyr::filter(.data$market == primary, .data$window == "60d")
  primary_regime <- vol_regime %>%
    dplyr::filter(.data$market == primary)
  primary_iv <- realized_vs_implied %>%
    dplyr::filter(.data$market == primary, is.finite(.data$atm_iv))
  primary_vov <- vol_of_vol_context %>%
    dplyr::filter(.data$market == primary)

  rv20_current <- if (nrow(primary_rv20) > 0L) primary_rv20$current_vol[[1]] else NA_real_
  rv60_current <- if (nrow(primary_rv60) > 0L) primary_rv60$current_vol[[1]] else NA_real_
  iv_current <- if (nrow(primary_iv) > 0L) utils::tail(primary_iv$atm_iv, 1L) else NA_real_
  iv_rv_diff <- if (is.finite(iv_current) && is.finite(rv60_current)) iv_current - rv60_current else NA_real_
  history_context_label <- ea_history_context_label(history_context)

  labels <- stats::setNames(catalog$label, catalog$market)
  kpis <- tibble::tribble(
    ~title, ~value, ~delta, ~status,
    "20d Realized",
    if (is.finite(rv20_current)) scales::percent(rv20_current, accuracy = 0.1) else "N/A",
    history_context_label,
    "neutral",
    "60d Realized",
    if (is.finite(rv60_current)) scales::percent(rv60_current, accuracy = 0.1) else "N/A",
    "annualized",
    "neutral",
    "ATM Implied",
    if (is.finite(iv_current)) scales::percent(iv_current, accuracy = 0.1) else "N/A",
    "front listed tenor",
    "neutral",
    "IV-RV Spread",
    if (is.finite(iv_rv_diff)) paste0(ifelse(iv_rv_diff >= 0, "+", ""), scales::percent(iv_rv_diff, accuracy = 0.1)) else "N/A",
    if (is.finite(iv_rv_diff) && iv_rv_diff > 0) "options rich" else "options cheap",
    "neutral",
    "Vol Percentile",
    if (nrow(primary_regime) > 0L && is.finite(primary_regime$vol_percentile[[1]])) scales::percent(primary_regime$vol_percentile[[1]], accuracy = 1) else "N/A",
    history_context_label,
    "neutral",
    "Vol Regime",
    if (nrow(primary_regime) > 0L) primary_regime$regime[[1]] else "N/A",
    labels[[primary]],
    "neutral"
  )

  notes <- c(
    "Time-series volatility views use the selected display window.",
    "Vol cones, percentiles, regimes, and rich-cheap context use the local history basis selector.",
    "Implied-vol diagnostics come from the packaged options surface built from ZEMA option quotes."
  )
  assumptions <- c(
    paste0("Volatility context uses the ", ea_history_context_label(history_context), " history window ending on the display-range end date."),
    "Current term-structure volatility uses trailing 20d, 60d, and 120d realized-vol estimates as of the selected end date.",
    "Vol-of-vol uses a 60-day rolling window on the 20d realized-vol series."
  )

  list(
    available_markets = available_markets,
    focus_market = primary,
    history_context = history_context,
    history_context_label = ea_history_context_label(history_context),
    display_window = display_window,
    context_window = context_window,
    latest_options_date = latest_options_date,
    realized_vol_timeseries = realized_vol_timeseries,
    garch_vol = garch_vol,
    vol_term_structure = vol_term_structure,
    vol_surface_grid = vol_surface_grid,
    vol_skew_snapshot = vol_skew_snapshot,
    realized_vs_implied = realized_vs_implied,
    vol_cone = vol_cone,
    vol_of_vol = vol_of_vol,
    vol_of_vol_context = vol_of_vol_context,
    vol_history_context = vol_history_context,
    cross_asset_vol = cross_asset_vol,
    vol_regime = vol_regime,
    iv_rv_spread = iv_rv_spread,
    kpis = kpis,
    notes = notes,
    assumptions = assumptions
  )
}
