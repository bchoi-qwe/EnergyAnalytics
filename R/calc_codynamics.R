# ---- Co-Dynamics Calculation Layer ----
# Cross-market correlations, spreads, betas, and PCA.

ea_parse_rolling_window <- function(window_str) {
  n <- as.integer(gsub("[^0-9]", "", window_str))
  if (is.na(n)) 63L else n
}

ea_calc_codynamics <- function(filters, history_context = "5y") {
  catalog <- ea_market_catalog()
  history_context <- ea_normalize_history_context(history_context)
  selected_markets <- ea_coalesce(filters$commodities, catalog$market)
  window <- ea_parse_rolling_window(ea_coalesce(filters$rolling_window, "63D"))

  empty_corr_matrix <- tibble::tibble(
    market_x = character(),
    market_y = character(),
    correlation = numeric(),
    corr_delta = numeric(),
    percentile = numeric(),
    beta = numeric(),
    r_squared = numeric()
  )
  empty_corr_ts <- tibble::tibble(
    date = as.Date(character()),
    pair = character(),
    correlation = numeric(),
    window = character(),
    se = numeric(),
    ci_lo = numeric(),
    ci_hi = numeric()
  )
  empty_spread_ts <- tibble::tibble(
    date = as.Date(character()),
    spread_label = character(),
    value = numeric()
  )
  empty_spread_summary <- tibble::tibble(
    spread_label = character(),
    current = numeric(),
    mean = numeric(),
    std = numeric(),
    zscore = numeric(),
    percentile = numeric()
  )
  empty_beta_matrix <- tibble::tibble(
    market_x = character(),
    market_y = character(),
    beta = numeric(),
    r_squared = numeric()
  )
  empty_pair_summary <- tibble::tibble(
    pair = character(),
    market_x = character(),
    market_y = character(),
    current_corr = numeric(),
    corr_delta = numeric(),
    corr_percentile = numeric(),
    beta = numeric(),
    r_squared = numeric(),
    residual_z = numeric()
  )
  empty_residual <- tibble::tibble(
    date = as.Date(character()),
    pair = character(),
    residual = numeric(),
    ou_mu = numeric(),
    ou_sigma = numeric(),
    band_1_lo = numeric(),
    band_1_hi = numeric(),
    band_2_lo = numeric(),
    band_2_hi = numeric()
  )
  empty_treasury_curve <- tibble::tibble(
    date = as.Date(character()),
    tenor = character(),
    tenor_years = numeric(),
    yield = numeric(),
    change_bps = numeric()
  )
  empty_treasury_betas <- tibble::tibble(
    market = character(),
    factor = character(),
    beta = numeric()
  )
  empty_pca <- tibble::tibble(
    market = character(),
    PC1 = numeric(),
    PC2 = numeric(),
    PC3 = numeric()
  )

  build_front_returns <- function(curves) {
    curves %>%
      dplyr::filter(.data$curve_point_num == 1) %>%
      dplyr::arrange(.data$market, .data$date) %>%
      dplyr::group_by(.data$market) %>%
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
      dplyr::filter(!is.na(.data$log_return), is.finite(.data$log_return)) %>%
      dplyr::select(.data$date, .data$market, .data$value, .data$log_return)
  }

  build_treasury_curve_snapshot <- function(anchor_date) {
    ust <- tryCatch(
      ea_load_dataset("ust_curve_long"),
      error = function(...) tibble::tibble()
    )

    if (!is.data.frame(ust) || nrow(ust) == 0L || !inherits(anchor_date, "Date") || is.na(anchor_date)) {
      return(empty_treasury_curve)
    }

    ust <- ust %>%
      dplyr::filter(.data$market == "UST", .data$date <= anchor_date) %>%
      dplyr::arrange(.data$date, .data$curve_point_num)

    if (nrow(ust) == 0L) {
      return(empty_treasury_curve)
    }

    latest_date <- max(ust$date, na.rm = TRUE)
    previous_date <- if (any(ust$date < latest_date, na.rm = TRUE)) {
      max(ust$date[ust$date < latest_date], na.rm = TRUE)
    } else {
      as.Date(NA)
    }

    latest_curve <- ust %>%
      dplyr::filter(.data$date == latest_date) %>%
      dplyr::transmute(
        date = .data$date,
        tenor = .data$curve_point,
        tenor_years = .data$curve_point_num,
        yield = .data$value
      )

    if (is.na(previous_date)) {
      return(latest_curve %>% dplyr::mutate(change_bps = NA_real_))
    }

    previous_curve <- ust %>%
      dplyr::filter(.data$date == previous_date) %>%
      dplyr::transmute(
        tenor = .data$curve_point,
        tenor_years = .data$curve_point_num,
        previous_yield = .data$value
      )

    latest_curve %>%
      dplyr::left_join(previous_curve, by = c("tenor", "tenor_years")) %>%
      dplyr::mutate(change_bps = (.data$yield - .data$previous_yield) * 100) %>%
      dplyr::select("date", "tenor", "tenor_years", "yield", "change_bps")
  }

  display_curves <- ea_load_filtered_curves(filters, apply_date_range = TRUE)
  all_curves <- ea_load_filtered_curves(filters, apply_date_range = FALSE)

  if (nrow(all_curves) == 0L || length(selected_markets) == 0L) {
    return(list(
      available_markets = character(),
      history_context = history_context,
      history_context_label = ea_history_context_label(history_context),
      display_window = as.Date(c(NA, NA)),
      context_window = as.Date(c(NA, NA)),
      correlation_matrix = empty_corr_matrix,
      correlation_timeseries = empty_corr_ts,
      spread_timeseries = empty_spread_ts,
      spread_zscore = empty_spread_summary,
      beta_matrix = empty_beta_matrix,
      pca_decomposition = empty_pca,
      treasury_curve_snapshot = empty_treasury_curve,
      treasury_betas = empty_treasury_betas,
      connectedness_score = NA_real_,
      correlation_breaks = empty_pair_summary,
      coint_residual = empty_residual,
      rolling_beta_ts = tibble::tibble(date = as.Date(character()), pair = character(), beta = numeric(), r_squared = numeric()),
      kpis = tibble::tibble(title = character(), value = character(), delta = character(), status = character()),
      notes = c("Co-dynamics requires at least two markets with overlapping front-contract history."),
      assumptions = c(paste0("Rolling window: ", window, " trading days."))
    ))
  }

  available_markets <- selected_markets[selected_markets %in% unique(all_curves$market)]
  if (length(available_markets) == 0L) {
    available_markets <- unique(all_curves$market)
  }

  all_curves <- all_curves %>%
    dplyr::filter(.data$market %in% available_markets) %>%
    dplyr::arrange(.data$market, .data$curve_point_num, .data$date)

  display_curves <- display_curves %>%
    dplyr::filter(.data$market %in% available_markets) %>%
    dplyr::arrange(.data$market, .data$curve_point_num, .data$date)

  if (nrow(display_curves) == 0L) {
    return(list(
      available_markets = available_markets,
      history_context = history_context,
      history_context_label = ea_history_context_label(history_context),
      display_window = as.Date(c(NA, NA)),
      context_window = as.Date(c(NA, NA)),
      correlation_matrix = empty_corr_matrix,
      correlation_timeseries = empty_corr_ts,
      spread_timeseries = empty_spread_ts,
      spread_zscore = empty_spread_summary,
      beta_matrix = empty_beta_matrix,
      pca_decomposition = empty_pca,
      treasury_curve_snapshot = empty_treasury_curve,
      treasury_betas = empty_treasury_betas,
      connectedness_score = NA_real_,
      correlation_breaks = empty_pair_summary,
      coint_residual = empty_residual,
      rolling_beta_ts = tibble::tibble(date = as.Date(character()), pair = character(), beta = numeric(), r_squared = numeric()),
      kpis = tibble::tibble(title = character(), value = character(), delta = character(), status = character()),
      notes = c("Co-dynamics charts use the selected display range. The current display range contains no data."),
      assumptions = c(paste0("Rolling window: ", window, " trading days."))
    ))
  }

  window_info <- ea_resolve_analysis_windows(display_curves$date, all_curves$date, history_context)
  display_window <- window_info$display_window
  context_window <- window_info$context_window
  anchor_date <- window_info$anchor_date

  context_curves <- all_curves %>%
    dplyr::filter(.data$date >= context_window[[1]], .data$date <= context_window[[2]])

  calc_curves <- all_curves %>%
    dplyr::filter(.data$date <= anchor_date)

  all_front <- build_front_returns(calc_curves)
  context_front <- all_front %>%
    dplyr::filter(.data$date >= context_window[[1]], .data$date <= context_window[[2]])

  treasury_curve_snapshot <- build_treasury_curve_snapshot(anchor_date)

  build_treasury_betas <- function() {
    if (nrow(context_front) == 0L || !inherits(anchor_date, "Date") || is.na(anchor_date)) {
      return(empty_treasury_betas)
    }

    tryCatch({
      ust <- ea_load_dataset("ust_curve_long") %>%
        dplyr::filter(.data$market == "UST", .data$date <= anchor_date) %>%
        dplyr::arrange(.data$date, .data$curve_point_num)

      if (nrow(ust) == 0L) {
        return(empty_treasury_betas)
      }

      ust_wide <- ust %>%
        dplyr::filter(.data$curve_point_num %in% c(2, 5, 10, 30)) %>%
        dplyr::select("date", "curve_point_num", "value") %>%
        tidyr::pivot_wider(names_from = "curve_point_num", values_from = "value") %>%
        dplyr::arrange(.data$date)

      if (!all(c("2", "5", "10", "30") %in% names(ust_wide))) {
        return(empty_treasury_betas)
      }

      ust_factors <- ust_wide %>%
        dplyr::mutate(
          level = (.data$`2` + .data$`5` + .data$`10` + .data$`30`) / 4,
          slope = .data$`10` - .data$`2`,
          curvature = .data$`30` - .data$`2` - 2 * (.data$`10` - .data$`2`)
        ) %>%
        dplyr::arrange(.data$date) %>%
        dplyr::mutate(
          level_chg = .data$level - dplyr::lag(.data$level),
          slope_chg = .data$slope - dplyr::lag(.data$slope),
          curvature_chg = .data$curvature - dplyr::lag(.data$curvature)
        ) %>%
        dplyr::filter(
          .data$date >= context_window[[1]],
          .data$date <= context_window[[2]],
          !is.na(.data$level_chg)
        ) %>%
        dplyr::select("date", "level_chg", "slope_chg", "curvature_chg")

      purrr::map_dfr(available_markets, function(mkt) {
        mkt_ret <- context_front %>%
          dplyr::filter(.data$market == mkt) %>%
          dplyr::select("date", "log_return")

        joined <- dplyr::inner_join(mkt_ret, ust_factors, by = "date") %>%
          stats::na.omit()

        if (nrow(joined) < 30L) {
          return(tibble::tibble(
            market = mkt,
            factor = c("Level", "Slope", "Curvature"),
            beta = NA_real_
          ))
        }

        fit <- stats::lm(log_return ~ level_chg + slope_chg + curvature_chg, data = joined)
        coefs <- stats::coef(fit)

        tibble::tibble(
          market = mkt,
          factor = c("Level", "Slope", "Curvature"),
          beta = as.numeric(coefs[c("level_chg", "slope_chg", "curvature_chg")])
        )
      })
    }, error = function(...) empty_treasury_betas)
  }

  treasury_betas <- build_treasury_betas()

  if (length(available_markets) < 2L) {
    return(list(
      available_markets = available_markets,
      history_context = history_context,
      history_context_label = ea_history_context_label(history_context),
      display_window = display_window,
      context_window = context_window,
      correlation_matrix = empty_corr_matrix,
      correlation_timeseries = empty_corr_ts,
      spread_timeseries = empty_spread_ts,
      spread_zscore = empty_spread_summary,
      beta_matrix = empty_beta_matrix,
      pca_decomposition = empty_pca,
      treasury_curve_snapshot = treasury_curve_snapshot,
      treasury_betas = treasury_betas,
      connectedness_score = NA_real_,
      correlation_breaks = empty_pair_summary,
      coint_residual = empty_residual,
      rolling_beta_ts = tibble::tibble(date = as.Date(character()), pair = character(), beta = numeric(), r_squared = numeric()),
      kpis = tibble::tibble(title = character(), value = character(), delta = character(), status = character()),
      notes = c("Co-dynamics requires at least two markets. Treasury curve and rate betas remain available with one market."),
      assumptions = c(paste0("Rolling window: ", window, " trading days."))
    ))
  }

  all_front_wide <- all_front %>%
    dplyr::select(.data$date, .data$market, .data$log_return) %>%
    tidyr::pivot_wider(names_from = "market", values_from = "log_return") %>%
    dplyr::arrange(.data$date)

  context_front_wide <- context_front %>%
    dplyr::select(.data$date, .data$market, .data$log_return) %>%
    tidyr::pivot_wider(names_from = "market", values_from = "log_return") %>%
    dplyr::arrange(.data$date)

  context_price_wide <- context_curves %>%
    dplyr::filter(.data$curve_point_num == 1) %>%
    dplyr::select(.data$date, .data$market, .data$value) %>%
    tidyr::pivot_wider(names_from = "market", values_from = "value") %>%
    dplyr::arrange(.data$date)

  pairs <- utils::combn(available_markets, 2, simplify = FALSE)

  pair_analysis <- purrr::map(pairs, function(pr) {
    pair_label <- paste(pr[1], "vs", pr[2])

    if (!all(c("date", pr) %in% names(all_front_wide))) {
      return(list(
        correlation_timeseries = empty_corr_ts,
        correlation_context = tibble::tibble(date = as.Date(character()), correlation = numeric(), n_obs = integer()),
        beta_matrix = tibble::tibble(market_x = pr[1], market_y = pr[2], beta = NA_real_, r_squared = NA_real_),
        rolling_beta_ts = tibble::tibble(date = as.Date(character()), pair = character(), beta = numeric(), r_squared = numeric(), n_obs = integer()),
        residual_ts = empty_residual,
        pair_summary = tibble::tibble(
          pair = pair_label,
          market_x = pr[1],
          market_y = pr[2],
          current_corr = NA_real_,
          corr_delta = NA_real_,
          corr_percentile = NA_real_,
          beta = NA_real_,
          beta_reverse = NA_real_,
          r_squared = NA_real_,
          residual_z = NA_real_
        )
      ))
    }

    pair_returns_all <- all_front_wide %>%
      dplyr::select(dplyr::all_of(c("date", pr))) %>%
      dplyr::arrange(.data$date)

    pair_returns_context <- if (all(c("date", pr) %in% names(context_front_wide))) {
      ea_complete_market_frame(context_front_wide, pr)
    } else {
      tibble::tibble()
    }

    rolling_stats <- rolling_pair_stats_cpp(
      x = pair_returns_all[[pr[1]]],
      y = pair_returns_all[[pr[2]]],
      window = window,
      min_obs = 10L
    )

    rolling_all <- tibble::tibble(
      date = pair_returns_all$date,
      correlation = rolling_stats$corr,
      beta = rolling_stats$beta,
      r_squared = rolling_stats$r_squared,
      n_obs = rolling_stats$n_obs
    )

    corr_context <- rolling_all %>%
      dplyr::transmute(date = .data$date, correlation = .data$correlation, n_obs = .data$n_obs) %>%
      dplyr::filter(
        .data$date >= context_window[[1]],
        .data$date <= context_window[[2]],
        !is.na(.data$correlation)
      )

    corr_ts <- corr_context %>%
      dplyr::filter(.data$date >= display_window[[1]], .data$date <= display_window[[2]]) %>%
      dplyr::mutate(pair = pair_label, window = paste0(window, "d")) %>%
      dplyr::select(.data$date, .data$pair, .data$correlation, .data$window, .data$n_obs)

    beta_ts <- rolling_all %>%
      dplyr::transmute(
        date = .data$date,
        pair = pair_label,
        beta = .data$beta,
        r_squared = .data$r_squared,
        n_obs = .data$n_obs
      ) %>%
      dplyr::filter(
        .data$date >= display_window[[1]],
        .data$date <= display_window[[2]],
        !is.na(.data$beta)
      )

    beta_fit_stats <- if (nrow(pair_returns_context) > 0L) {
      ea_simple_regression_stats(
        x = pair_returns_context[[pr[1]]],
        y = pair_returns_context[[pr[2]]],
        min_obs = 30L
      )
    } else {
      ea_simple_regression_stats(numeric(), numeric(), min_obs = 30L)
    }
    beta_fit <- tibble::tibble(
      market_x = pr[1],
      market_y = pr[2],
      beta = beta_fit_stats$beta,
      r_squared = beta_fit_stats$r_squared
    )

    beta_reverse_fit_stats <- if (nrow(pair_returns_context) > 0L) {
      ea_simple_regression_stats(
        x = pair_returns_context[[pr[2]]],
        y = pair_returns_context[[pr[1]]],
        min_obs = 30L
      )
    } else {
      ea_simple_regression_stats(numeric(), numeric(), min_obs = 30L)
    }

    pair_prices_context <- if (all(c("date", pr) %in% names(context_price_wide))) {
      ea_complete_market_frame(context_price_wide, pr)
    } else {
      tibble::tibble()
    }
    price_fit <- if (nrow(pair_prices_context) > 0L) {
      ea_simple_regression_stats(
        x = pair_prices_context[[pr[1]]],
        y = pair_prices_context[[pr[2]]],
        min_obs = 60L
      )
    } else {
      ea_simple_regression_stats(numeric(), numeric(), min_obs = 60L)
    }

    residual_ts <- if (is.finite(price_fit$beta) && length(price_fit$residuals) > 0L) {
      resid_vec <- price_fit$residuals
      ou <- tryCatch(RTL::fitOU(spread = resid_vec), error = function(...) NULL)

      if (is.null(ou)) {
        empty_residual
      } else {
        ou_stationary_sd <- ea_ou_stationary_sd(ou$theta, ou$sigma)
        tibble::tibble(
          date = pair_prices_context$date,
          pair = pair_label,
          residual = resid_vec,
          ou_mu = ou$mu,
          ou_sigma = ou_stationary_sd,
          band_1_lo = ou$mu - ou_stationary_sd,
          band_1_hi = ou$mu + ou_stationary_sd,
          band_2_lo = ou$mu - 2 * ou_stationary_sd,
          band_2_hi = ou$mu + 2 * ou_stationary_sd
        ) %>%
          dplyr::filter(.data$date >= display_window[[1]], .data$date <= display_window[[2]])
      }
    } else {
      empty_residual
    }

    current_corr <- if (nrow(corr_ts) > 0L) utils::tail(corr_ts$correlation, 1L) else if (nrow(corr_context) > 0L) utils::tail(corr_context$correlation, 1L) else NA_real_
    corr_delta <- if (nrow(corr_ts) >= 21L) current_corr - corr_ts$correlation[[nrow(corr_ts) - 20L]] else NA_real_
    corr_percentile <- if (nrow(corr_context) > 0L && is.finite(current_corr)) mean(corr_context$correlation <= current_corr, na.rm = TRUE) else NA_real_
    residual_z <- if (nrow(residual_ts) > 0L) {
      latest_row <- utils::tail(residual_ts, 1L)
      band_width <- latest_row$band_1_hi[[1]] - latest_row$ou_mu[[1]]
      if (is.finite(band_width) && band_width != 0) {
        (latest_row$residual[[1]] - latest_row$ou_mu[[1]]) / band_width
      } else {
        NA_real_
      }
    } else {
      NA_real_
    }

    list(
      correlation_timeseries = corr_ts,
      correlation_context = corr_context,
      beta_matrix = beta_fit,
      rolling_beta_ts = beta_ts,
      residual_ts = residual_ts,
      pair_summary = tibble::tibble(
        pair = pair_label,
        market_x = pr[1],
        market_y = pr[2],
        current_corr = current_corr,
        corr_delta = corr_delta,
        corr_percentile = corr_percentile,
        beta = beta_fit$beta[[1]],
        beta_reverse = beta_reverse_fit_stats$beta,
        r_squared = beta_fit$r_squared[[1]],
        residual_z = residual_z
      )
    )
  })

  correlation_timeseries <- dplyr::bind_rows(purrr::map(pair_analysis, "correlation_timeseries")) %>%
    dplyr::mutate(
      se = dplyr::if_else(
        .data$n_obs > 3L,
        1 / sqrt(.data$n_obs - 3L),
        NA_real_
      ),
      fisher_z = dplyr::if_else(
        is.finite(.data$correlation),
        atanh(pmin(pmax(.data$correlation, -0.999999), 0.999999)),
        NA_real_
      ),
      ci_lo = dplyr::if_else(
        is.finite(.data$se) & is.finite(.data$fisher_z),
        tanh(.data$fisher_z - 1.96 * .data$se),
        NA_real_
      ),
      ci_hi = dplyr::if_else(
        is.finite(.data$se) & is.finite(.data$fisher_z),
        tanh(.data$fisher_z + 1.96 * .data$se),
        NA_real_
      )
    )

  beta_matrix <- dplyr::bind_rows(purrr::map(pair_analysis, "beta_matrix"))
  rolling_beta_ts <- dplyr::bind_rows(purrr::map(pair_analysis, "rolling_beta_ts"))
  coint_residual <- dplyr::bind_rows(purrr::map(pair_analysis, "residual_ts"))
  pair_summary <- dplyr::bind_rows(purrr::map(pair_analysis, "pair_summary"))

  correlation_breaks <- pair_summary %>%
    dplyr::arrange(dplyr::desc(abs(.data$corr_delta)))

  correlation_matrix <- dplyr::bind_rows(
    pair_summary %>%
      dplyr::transmute(
        market_x = .data$market_x,
        market_y = .data$market_y,
        correlation = .data$current_corr,
        corr_delta = .data$corr_delta,
        percentile = .data$corr_percentile,
        beta = .data$beta,
        r_squared = .data$r_squared
      ),
    pair_summary %>%
      dplyr::transmute(
        market_x = .data$market_y,
        market_y = .data$market_x,
        correlation = .data$current_corr,
        corr_delta = .data$corr_delta,
        percentile = .data$corr_percentile,
        beta = .data$beta_reverse,
        r_squared = .data$r_squared
      ),
    tibble::tibble(
      market_x = available_markets,
      market_y = available_markets,
      correlation = 1,
      corr_delta = 0,
      percentile = 1,
      beta = 1,
      r_squared = 1
    )
  )

  connectedness_score <- if (nrow(pair_summary) > 0L) {
    mean(abs(pair_summary$current_corr), na.rm = TRUE)
  } else {
    NA_real_
  }

  front_prices <- calc_curves %>%
    dplyr::filter(.data$curve_point_num == 1) %>%
    dplyr::select(.data$date, .data$market, .data$value)

  spread_definitions <- list(
    "CL-BRN" = c("CL", "BRN"),
    "CL-RB (Crack)" = c("RB", "CL"),
    "CL-HO (Crack)" = c("HO", "CL")
  )

  spread_timeseries <- purrr::map_dfr(names(spread_definitions), function(spread_label) {
    legs <- spread_definitions[[spread_label]]
    if (!all(legs %in% available_markets)) {
      return(tibble::tibble())
    }

    spread_series <- front_prices %>%
      dplyr::filter(.data$market %in% legs) %>%
      tidyr::pivot_wider(names_from = "market", values_from = "value") %>%
      dplyr::arrange(.data$date) %>%
      dplyr::filter(!is.na(.data[[legs[[1]]]]), !is.na(.data[[legs[[2]]]])) %>%
      dplyr::transmute(
        date = .data$date,
        spread_label = spread_label,
        value = .data[[legs[[1]]]] - .data[[legs[[2]]]]
      )

    spread_series %>%
      dplyr::filter(.data$date >= display_window[[1]], .data$date <= display_window[[2]])
  })

  spread_zscore <- purrr::map_dfr(names(spread_definitions), function(spread_label) {
    legs <- spread_definitions[[spread_label]]
    if (!all(legs %in% available_markets)) {
      return(tibble::tibble())
    }

    spread_series <- front_prices %>%
      dplyr::filter(.data$market %in% legs, .data$date >= context_window[[1]], .data$date <= context_window[[2]]) %>%
      tidyr::pivot_wider(names_from = "market", values_from = "value") %>%
      dplyr::arrange(.data$date) %>%
      dplyr::filter(!is.na(.data[[legs[[1]]]]), !is.na(.data[[legs[[2]]]])) %>%
      dplyr::transmute(
        date = .data$date,
        value = .data[[legs[[1]]]] - .data[[legs[[2]]]]
      )

    if (nrow(spread_series) == 0L) {
      return(tibble::tibble())
    }

    current_display <- spread_timeseries %>%
      dplyr::filter(.data$spread_label == spread_label) %>%
      dplyr::arrange(.data$date)

    current_value <- if (nrow(current_display) > 0L) utils::tail(current_display$value, 1L) else utils::tail(spread_series$value, 1L)
    spread_mean <- mean(spread_series$value, na.rm = TRUE)
    spread_sd <- stats::sd(spread_series$value, na.rm = TRUE)

    tibble::tibble(
      spread_label = spread_label,
      current = current_value,
      mean = spread_mean,
      std = spread_sd,
      zscore = if (is.finite(spread_sd) && spread_sd > 0) (current_value - spread_mean) / spread_sd else NA_real_,
      percentile = mean(spread_series$value <= current_value, na.rm = TRUE)
    )
  })

  context_returns_wide <- context_front %>%
    dplyr::select(.data$date, .data$market, .data$log_return) %>%
    tidyr::pivot_wider(names_from = "market", values_from = "log_return") %>%
    dplyr::arrange(.data$date)

  pca_decomposition <- tryCatch({
    pca_input <- context_returns_wide %>%
      dplyr::select(dplyr::all_of(available_markets)) %>%
      tidyr::drop_na()

    if (nrow(pca_input) < 2L) {
      stop("insufficient complete rows for PCA")
    }

    pca <- stats::prcomp(as.matrix(pca_input), center = TRUE, scale. = TRUE)
    loadings <- as.data.frame(pca$rotation[, 1:min(3, ncol(pca$rotation)), drop = FALSE])
    colnames(loadings) <- paste0("PC", seq_len(ncol(loadings)))
    loadings$market <- rownames(loadings)

    for (pc in c("PC1", "PC2", "PC3")) {
      if (!pc %in% names(loadings)) {
        loadings[[pc]] <- NA_real_
      }
    }

    tibble::as_tibble(loadings) %>%
      dplyr::select("market", "PC1", "PC2", "PC3")
  }, error = function(...) {
    tibble::tibble(market = available_markets, PC1 = NA_real_, PC2 = NA_real_, PC3 = NA_real_)
  })

  biggest_break <- correlation_breaks %>%
    dplyr::slice_max(abs(.data$corr_delta), n = 1, with_ties = FALSE)

  top_spread <- spread_zscore %>%
    dplyr::slice_max(abs(.data$zscore), n = 1, with_ties = FALSE)

  cl_brn_corr <- correlation_matrix %>%
    dplyr::filter(.data$market_x == "CL", .data$market_y == "BRN") %>%
    dplyr::pull(.data$correlation)
  cl_ng_corr <- correlation_matrix %>%
    dplyr::filter((.data$market_x == "CL" & .data$market_y == "NG") | (.data$market_x == "NG" & .data$market_y == "CL")) %>%
    dplyr::pull(.data$correlation)
  crack_val <- spread_zscore %>%
    dplyr::filter(grepl("RB", .data$spread_label) | grepl("Crack", .data$spread_label)) %>%
    dplyr::slice(1) %>%
    dplyr::pull(.data$current)

  kpis <- tibble::tribble(
    ~title, ~value, ~delta, ~status,
    "CL-BRN Corr",
    if (length(cl_brn_corr) > 0L && is.finite(cl_brn_corr[[1]])) scales::number(cl_brn_corr[[1]], accuracy = 0.01) else "N/A",
    paste0(window, "d @ ", ea_history_context_label(history_context)),
    "neutral",

    "Crack Spread",
    if (length(crack_val) > 0L && is.finite(crack_val[[1]])) scales::number(crack_val[[1]], accuracy = 0.01) else "N/A",
    "display end",
    "neutral",

    "CL-NG Corr",
    if (length(cl_ng_corr) > 0L && is.finite(cl_ng_corr[[1]])) scales::number(cl_ng_corr[[1]], accuracy = 0.01) else "N/A",
    paste0(window, "d @ ", ea_history_context_label(history_context)),
    "neutral",

    "Connectedness",
    if (is.finite(connectedness_score)) scales::number(connectedness_score, accuracy = 0.01) else "N/A",
    "mean abs corr",
    "neutral",

    "Largest Corr Break",
    if (nrow(biggest_break) > 0L && is.finite(biggest_break$corr_delta[[1]])) scales::number(biggest_break$corr_delta[[1]], accuracy = 0.01) else "N/A",
    if (nrow(biggest_break) > 0L) biggest_break$pair[[1]] else "",
    "neutral",

    "Spread Outlier",
    if (nrow(top_spread) > 0L && is.finite(top_spread$zscore[[1]])) scales::number(top_spread$zscore[[1]], accuracy = 0.1) else "N/A",
    if (nrow(top_spread) > 0L) top_spread$spread_label[[1]] else "",
    "neutral"
  )

  list(
    available_markets = available_markets,
    history_context = history_context,
    history_context_label = ea_history_context_label(history_context),
    display_window = display_window,
    context_window = context_window,
    correlation_matrix = correlation_matrix,
    correlation_timeseries = correlation_timeseries,
    spread_timeseries = spread_timeseries,
    spread_zscore = spread_zscore,
    beta_matrix = beta_matrix,
    pca_decomposition = pca_decomposition,
    treasury_curve_snapshot = treasury_curve_snapshot,
    treasury_betas = treasury_betas,
    connectedness_score = connectedness_score,
    correlation_breaks = correlation_breaks,
    coint_residual = coint_residual,
    rolling_beta_ts = rolling_beta_ts,
    kpis = kpis,
    notes = c(
      "Rolling pair charts use the selected display range, but they are computed with enough pre-window history to avoid empty warm-up periods.",
      paste0("Current correlation, spread z-scores, and rate betas use the ", ea_history_context_label(history_context), " history window ending on the display-range end date."),
      "Treasury factor betas come from OLS regressions on UST level, slope, and curvature changes."
    ),
    assumptions = c(
      paste0("Rolling window: ", window, " trading days."),
      "Pair analytics use pair-complete data only, so unrelated markets do not change a given pair's statistics.",
      "Spread rich/cheap context is measured against the selected history basis, not just the displayed slice."
    )
  )
}
