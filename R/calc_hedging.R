# ---- Hedging Calculation Layer ----
# Hedge ratios, rolling betas, effectiveness, and mean reversion.

ea_calc_hedging <- function(filters, history_context = "5y") {
  catalog <- ea_market_catalog()
  history_context <- ea_normalize_history_context(history_context)
  markets <- ea_coalesce(filters$commodities, c("CL", "BRN", "RB"))
  benchmark <- ea_coalesce(filters$comparison_commodity, "CL")
  window <- ea_parse_rolling_window(ea_coalesce(filters$rolling_window, "63D"))
  annualize <- sqrt(252)

  targets <- setdiff(markets, benchmark)
  all_mkts <- unique(c(targets, benchmark))

  empty_ratios <- tibble::tibble(market = character(), beta = numeric(), r_squared = numeric(), std_error = numeric())
  empty_rolling <- tibble::tibble(date = as.Date(character()), market = character(), beta = numeric(), r_squared = numeric())
  empty_effectiveness <- tibble::tibble(market = character(), unhedged_vol = numeric(), hedged_vol = numeric(), vol_reduction_pct = numeric())
  empty_residual <- tibble::tibble(date = as.Date(character()), market = character(), residual = numeric())
  empty_ou <- tibble::tibble(market = character(), theta = numeric(), mu = numeric(), sigma = numeric(), half_life = numeric())
  empty_cross <- tibble::tibble(market_x = character(), market_y = character(), beta = numeric(), r_squared = numeric())
  empty_tenor <- tibble::tibble(market = character(), tenor = numeric(), beta = numeric(), r_squared = numeric())
  empty_stability <- tibble::tibble(market = character(), beta_sd = numeric())
  empty_cost <- tibble::tibble(market = character(), roll_cost = numeric(), basis_risk_cost = numeric())
  empty_stress <- tibble::tibble(market = character(), r2_normal = numeric(), r2_stress = numeric())
  empty_bands <- tibble::tibble(date = as.Date(character()), market = character(), residual = numeric(), ou_mu = numeric(), band_1_lo = numeric(), band_1_hi = numeric(), band_2_lo = numeric(), band_2_hi = numeric())

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

  curve_filters <- filters
  curve_filters$commodities <- all_mkts

  display_curves <- ea_load_filtered_curves(curve_filters, apply_date_range = TRUE)
  all_curves <- ea_load_filtered_curves(curve_filters, apply_date_range = FALSE)

  if (nrow(all_curves) == 0L || !benchmark %in% unique(all_curves$market) || length(targets) == 0L) {
    return(list(
      available_markets = markets,
      history_context = history_context,
      history_context_label = ea_history_context_label(history_context),
      display_window = as.Date(c(NA, NA)),
      context_window = as.Date(c(NA, NA)),
      hedge_ratios = empty_ratios,
      rolling_beta = empty_rolling,
      hedge_effectiveness = empty_effectiveness,
      residual_timeseries = empty_residual,
      ou_fit = empty_ou,
      cross_hedge_matrix = empty_cross,
      per_tenor_ratios = empty_tenor,
      ratio_stability = empty_stability,
      hedge_cost = empty_cost,
      stress_period_comparison = empty_stress,
      residual_with_bands = empty_bands,
      kpis = tibble::tibble(title = character(), value = character(), delta = character(), status = character()),
      notes = c("Cross-hedging requires a benchmark plus at least one target with overlapping history."),
      assumptions = c(paste0("Rolling window: ", window, " trading days."))
    ))
  }

  available_markets <- unique(c(targets[targets %in% unique(all_curves$market)], benchmark))
  targets <- setdiff(available_markets, benchmark)

  display_curves <- display_curves %>%
    dplyr::filter(.data$market %in% available_markets) %>%
    dplyr::arrange(.data$market, .data$curve_point_num, .data$date)

  all_curves <- all_curves %>%
    dplyr::filter(.data$market %in% available_markets) %>%
    dplyr::arrange(.data$market, .data$curve_point_num, .data$date)

  if (nrow(display_curves) == 0L) {
    return(list(
      available_markets = available_markets,
      history_context = history_context,
      history_context_label = ea_history_context_label(history_context),
      display_window = as.Date(c(NA, NA)),
      context_window = as.Date(c(NA, NA)),
      hedge_ratios = empty_ratios,
      rolling_beta = empty_rolling,
      hedge_effectiveness = empty_effectiveness,
      residual_timeseries = empty_residual,
      ou_fit = empty_ou,
      cross_hedge_matrix = empty_cross,
      per_tenor_ratios = empty_tenor,
      ratio_stability = empty_stability,
      hedge_cost = empty_cost,
      stress_period_comparison = empty_stress,
      residual_with_bands = empty_bands,
      kpis = tibble::tibble(title = character(), value = character(), delta = character(), status = character()),
      notes = c("Hedging charts use the selected display range. The current display range contains no data."),
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

  all_returns <- build_front_returns(calc_curves)
  context_returns <- all_returns %>%
    dplyr::filter(.data$date >= context_window[[1]], .data$date <= context_window[[2]])

  all_returns_wide <- all_returns %>%
    dplyr::select(.data$date, .data$market, .data$log_return) %>%
    tidyr::pivot_wider(names_from = "market", values_from = "log_return") %>%
    dplyr::arrange(.data$date)

  context_returns_wide <- context_returns %>%
    dplyr::select(.data$date, .data$market, .data$log_return) %>%
    tidyr::pivot_wider(names_from = "market", values_from = "log_return") %>%
    dplyr::arrange(.data$date)

  pair_frame_from_wide <- function(wide_frame, target_market, benchmark_market = benchmark, complete = FALSE) {
    cols <- c("date", target_market, benchmark_market)
    if (!all(cols %in% names(wide_frame))) {
      return(tibble::tibble())
    }

    out <- wide_frame %>%
      dplyr::select(dplyr::all_of(cols)) %>%
      dplyr::arrange(.data$date)

    if (isTRUE(complete)) {
      out <- out %>%
        dplyr::filter(stats::complete.cases(dplyr::pick(dplyr::all_of(c(target_market, benchmark_market)))))
    }

    out
  }

  context_prices_wide <- context_curves %>%
    dplyr::filter(.data$curve_point_num == 1) %>%
    dplyr::select(.data$date, .data$market, .data$value) %>%
    tidyr::pivot_wider(names_from = "market", values_from = "value") %>%
    dplyr::arrange(.data$date)

  target_analysis <- purrr::map(targets, function(mkt) {
    pair_all <- pair_frame_from_wide(all_returns_wide, mkt, complete = FALSE)
    pair_context <- pair_frame_from_wide(context_returns_wide, mkt, complete = TRUE)
    price_context <- pair_frame_from_wide(context_prices_wide, mkt, complete = TRUE)

    fit_stats <- if (nrow(pair_context) >= 20L) {
      ea_simple_regression_stats(
        x = pair_context[[benchmark]],
        y = pair_context[[mkt]],
        min_obs = 20L
      )
    } else {
      ea_simple_regression_stats(numeric(), numeric(), min_obs = 20L)
    }

    price_fit <- if (nrow(price_context) >= 60L) {
      ea_simple_regression_stats(
        x = price_context[[benchmark]],
        y = price_context[[mkt]],
        min_obs = 60L
      )
    } else {
      ea_simple_regression_stats(numeric(), numeric(), min_obs = 60L)
    }

    rolling_df <- if (nrow(pair_all) > 0L) {
      rolling_stats <- rolling_pair_stats_cpp(
        x = pair_all[[benchmark]],
        y = pair_all[[mkt]],
        window = window,
        min_obs = 10L
      )

      tibble::tibble(
        date = pair_all$date,
        market = mkt,
        beta = rolling_stats$beta,
        r_squared = rolling_stats$r_squared,
        n_obs = rolling_stats$n_obs
      )
    } else {
      tibble::tibble(date = as.Date(character()), market = character(), beta = numeric(), r_squared = numeric(), n_obs = integer())
    }

    list(
      market = mkt,
      pair_all = pair_all,
      pair_context = pair_context,
      price_context = price_context,
      fit = fit_stats,
      price_fit = price_fit,
      rolling = rolling_df
    )
  })
  names(target_analysis) <- targets

  pair_complete_counts <- purrr::map_int(target_analysis, ~ .x$fit$n_obs)
  if (length(targets) == 0L || max(pair_complete_counts, 0L) < 20L) {
    return(list(
      available_markets = available_markets,
      history_context = history_context,
      history_context_label = ea_history_context_label(history_context),
      display_window = display_window,
      context_window = context_window,
      hedge_ratios = empty_ratios,
      rolling_beta = empty_rolling,
      hedge_effectiveness = empty_effectiveness,
      residual_timeseries = empty_residual,
      ou_fit = empty_ou,
      cross_hedge_matrix = empty_cross,
      per_tenor_ratios = empty_tenor,
      ratio_stability = empty_stability,
      hedge_cost = empty_cost,
      stress_period_comparison = empty_stress,
      residual_with_bands = empty_bands,
      kpis = tibble::tibble(title = character(), value = character(), delta = character(), status = character()),
      notes = c("Cross-hedging needs at least one target with enough overlapping benchmark history inside the selected context window."),
      assumptions = c(paste0("Rolling window: ", window, " trading days."))
    ))
  }

  rolling_beta_context_full <- purrr::map_dfr(target_analysis, function(item) {
    item$rolling %>%
      dplyr::filter(
        .data$date >= context_window[[1]],
        .data$date <= context_window[[2]],
        !is.na(.data$beta)
      )
  })

  rolling_beta <- rolling_beta_context_full %>%
    dplyr::filter(.data$date >= display_window[[1]], .data$date <= display_window[[2]])

  hedge_ratios <- purrr::map_dfr(target_analysis, function(item) {
    tibble::tibble(
      market = item$market,
      beta = item$fit$beta,
      r_squared = item$fit$r_squared,
      std_error = item$fit$std_error
    )
  })

  hedge_effectiveness <- purrr::map_dfr(target_analysis, function(item) {
    if (!is.finite(item$fit$beta) || nrow(item$pair_context) < 20L) {
      return(tibble::tibble(market = item$market, unhedged_vol = NA_real_, hedged_vol = NA_real_, vol_reduction_pct = NA_real_))
    }

    resid <- item$fit$residuals
    unhedged <- stats::sd(item$pair_context[[item$market]], na.rm = TRUE) * annualize
    hedged <- stats::sd(resid, na.rm = TRUE) * annualize

    tibble::tibble(
      market = item$market,
      unhedged_vol = unhedged,
      hedged_vol = hedged,
      vol_reduction_pct = 1 - hedged / unhedged
    )
  })

  residual_context <- purrr::map_dfr(target_analysis, function(item) {
    if (!length(item$price_fit$residuals) || nrow(item$price_context) == 0L) {
      return(tibble::tibble())
    }

    tibble::tibble(
      date = item$price_context$date,
      market = item$market,
      residual = as.numeric(item$price_fit$residuals)
    )
  })

  residual_timeseries <- residual_context %>%
    dplyr::filter(.data$date >= display_window[[1]], .data$date <= display_window[[2]])

  ou_fit <- purrr::map_dfr(targets, function(mkt) {
    resid <- residual_context %>%
      dplyr::filter(.data$market == mkt) %>%
      dplyr::pull(.data$residual)

    if (length(resid) < 30L) {
      return(tibble::tibble(market = mkt, theta = NA_real_, mu = NA_real_, sigma = NA_real_, half_life = NA_real_))
    }

    tryCatch({
      ou <- RTL::fitOU(spread = resid)
      tibble::tibble(
        market = mkt,
        theta = ou$theta,
        mu = ou$mu,
        sigma = ou$sigma,
        half_life = ea_ou_half_life_days(ou$theta)
      )
    }, error = function(...) {
      tibble::tibble(market = mkt, theta = NA_real_, mu = NA_real_, sigma = NA_real_, half_life = NA_real_)
    })
  })

  residual_with_bands <- purrr::map_dfr(targets, function(mkt) {
    resid_df <- residual_context %>% dplyr::filter(.data$market == mkt)
    ou_params <- ou_fit %>% dplyr::filter(.data$market == mkt)

    if (nrow(resid_df) == 0L) {
      return(tibble::tibble())
    }

    if (nrow(ou_params) == 0L || is.na(ou_params$mu[[1]])) {
      return(resid_df %>%
        dplyr::mutate(
          ou_mu = NA_real_,
          band_1_lo = NA_real_,
          band_1_hi = NA_real_,
          band_2_lo = NA_real_,
          band_2_hi = NA_real_
        ) %>%
        dplyr::filter(.data$date >= display_window[[1]], .data$date <= display_window[[2]]))
    }

    ou_mu <- ou_params$mu[[1]]
    ou_sig <- ea_ou_stationary_sd(ou_params$theta[[1]], ou_params$sigma[[1]])

    resid_df %>%
      dplyr::mutate(
        ou_mu = ou_mu,
        band_1_lo = ou_mu - ou_sig,
        band_1_hi = ou_mu + ou_sig,
        band_2_lo = ou_mu - 2 * ou_sig,
        band_2_hi = ou_mu + 2 * ou_sig
      ) %>%
      dplyr::filter(.data$date >= display_window[[1]], .data$date <= display_window[[2]])
  })

  all_pairs <- utils::combn(available_markets, 2, simplify = FALSE)
  cross_hedge_matrix <- purrr::map_dfr(all_pairs, function(pr) {
    if (!all(c("date", pr) %in% names(context_returns_wide))) {
      return(tibble::tibble(market_x = pr[1], market_y = pr[2], beta = NA_real_, r_squared = NA_real_))
    }

    df <- ea_complete_market_frame(context_returns_wide, pr)
    fit_stats <- ea_simple_regression_stats(
      x = df[[pr[1]]],
      y = df[[pr[2]]],
      min_obs = 20L
    )

    if (!is.finite(fit_stats$beta)) {
      return(tibble::tibble(market_x = pr[1], market_y = pr[2], beta = NA_real_, r_squared = NA_real_))
    }

    tibble::tibble(
      market_x = pr[1],
      market_y = pr[2],
      beta = fit_stats$beta,
      r_squared = fit_stats$r_squared
    )
  })

  tenor_returns <- context_curves %>%
    dplyr::arrange(.data$market, .data$curve_point_num, .data$date) %>%
    dplyr::group_by(.data$market, .data$curve_point_num) %>%
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
    dplyr::filter(!is.na(.data$log_return)) %>%
    dplyr::select(.data$date, .data$market, .data$curve_point_num, .data$log_return)

  tenor_returns_wide <- tenor_returns %>%
    tidyr::pivot_wider(
      id_cols = c("date", "curve_point_num"),
      names_from = "market",
      values_from = "log_return"
    ) %>%
    dplyr::arrange(.data$curve_point_num, .data$date)

  tenor_splits <- split(tenor_returns_wide, tenor_returns_wide$curve_point_num)
  per_tenor_ratios <- purrr::imap_dfr(tenor_splits, function(df, tenor_key) {
    tenor <- suppressWarnings(as.integer(tenor_key))

    purrr::map_dfr(targets, function(mkt) {
      if (!all(c("date", benchmark, mkt) %in% names(df))) {
        return(tibble::tibble(market = mkt, tenor = tenor, beta = NA_real_, r_squared = NA_real_))
      }

      pair_df <- ea_complete_market_frame(df, c(benchmark, mkt))
      fit_stats <- ea_simple_regression_stats(
        x = pair_df[[benchmark]],
        y = pair_df[[mkt]],
        min_obs = 20L
      )

      tibble::tibble(
        market = mkt,
        tenor = tenor,
        beta = fit_stats$beta,
        r_squared = fit_stats$r_squared
      )
    })
  })

  ratio_stability <- rolling_beta_context_full %>%
    dplyr::group_by(.data$market) %>%
    dplyr::summarise(beta_sd = stats::sd(.data$beta, na.rm = TRUE), .groups = "drop")

  latest_curves <- all_curves %>%
    dplyr::filter(.data$date <= anchor_date) %>%
    dplyr::group_by(.data$market) %>%
    dplyr::filter(.data$date == max(.data$date, na.rm = TRUE)) %>%
    dplyr::ungroup()

  hedge_cost <- hedge_ratios %>%
    dplyr::transmute(market = .data$market, r_squared = .data$r_squared) %>%
    dplyr::left_join(
      latest_curves %>%
        dplyr::filter(.data$curve_point_num %in% c(1, 2)) %>%
        dplyr::select(.data$market, .data$curve_point_num, .data$value) %>%
        tidyr::pivot_wider(names_from = "curve_point_num", values_from = "value", names_prefix = "m") %>%
        dplyr::transmute(
          market = .data$market,
          roll_cost = dplyr::if_else(
            !is.na(.data$m1) & !is.na(.data$m2) & .data$m1 != 0,
            abs((.data$m1 - .data$m2) / .data$m1) * 12,
            NA_real_
          )
        ),
      by = "market"
    ) %>%
    dplyr::transmute(
      market = .data$market,
      roll_cost = .data$roll_cost,
      basis_risk_cost = 1 - .data$r_squared
    )

  benchmark_series <- all_returns %>%
    dplyr::filter(.data$market == benchmark) %>%
    dplyr::select(.data$date, benchmark = .data$log_return) %>%
    dplyr::arrange(.data$date) %>%
    dplyr::mutate(
      benchmark_vol = slider::slide_dbl(
        .data$benchmark,
        ~ {
          vals <- stats::na.omit(.x)
          if (length(vals) < 10L) return(NA_real_)
          stats::sd(vals) * annualize
        },
        .before = 19L,
        .complete = TRUE
      )
    ) %>%
    dplyr::filter(.data$date >= context_window[[1]], .data$date <= context_window[[2]])

  stress_cutoff <- stats::quantile(benchmark_series$benchmark_vol, 0.75, na.rm = TRUE)

  stress_period_comparison <- purrr::map_dfr(target_analysis, function(item) {
    if (nrow(item$pair_context) == 0L) {
      return(tibble::tibble(market = item$market, r2_normal = NA_real_, r2_stress = NA_real_))
    }

    df <- item$pair_context %>%
      dplyr::rename(y = dplyr::all_of(item$market), x = dplyr::all_of(benchmark)) %>%
      dplyr::left_join(
        benchmark_series %>% dplyr::select(.data$date, .data$benchmark_vol),
        by = "date"
      ) %>%
      dplyr::filter(!is.na(.data$benchmark_vol))

    if (nrow(df) < 40L || !is.finite(stress_cutoff)) {
      return(tibble::tibble(market = item$market, r2_normal = NA_real_, r2_stress = NA_real_))
    }

    normal_df <- df %>% dplyr::filter(.data$benchmark_vol < stress_cutoff)
    stress_df <- df %>% dplyr::filter(.data$benchmark_vol >= stress_cutoff)
    normal_fit <- ea_simple_regression_stats(normal_df$x, normal_df$y, min_obs = 20L)
    stress_fit <- ea_simple_regression_stats(stress_df$x, stress_df$y, min_obs = 20L)

    tibble::tibble(
      market = item$market,
      r2_normal = normal_fit$r_squared,
      r2_stress = stress_fit$r_squared
    )
  })

  best_proxy <- cross_hedge_matrix %>%
    dplyr::filter(.data$market_x != .data$market_y) %>%
    dplyr::slice_max(.data$r_squared, n = 1, with_ties = FALSE)

  primary_target <- if (length(targets) > 0L) targets[[1]] else NA_character_
  primary_hedge <- hedge_ratios %>% dplyr::filter(.data$market == primary_target)
  primary_eff <- hedge_effectiveness %>% dplyr::filter(.data$market == primary_target)
  primary_ou <- ou_fit %>% dplyr::filter(.data$market == primary_target)
  primary_stab <- ratio_stability %>% dplyr::filter(.data$market == primary_target)

  kpis <- tibble::tribble(
    ~title, ~value, ~delta, ~status,
    "Best Proxy",
    if (nrow(best_proxy) > 0L) paste(best_proxy$market_x[[1]], "vs", best_proxy$market_y[[1]]) else "N/A",
    if (nrow(best_proxy) > 0L) paste0("R\u00b2=", scales::number(best_proxy$r_squared[[1]], accuracy = 0.01)) else "",
    "neutral",

    "Hedge Ratio",
    if (nrow(primary_hedge) > 0L && is.finite(primary_hedge$beta[[1]])) scales::number(primary_hedge$beta[[1]], accuracy = 0.01) else "N/A",
    paste0("vs ", benchmark),
    "neutral",

    "Effectiveness",
    if (nrow(primary_eff) > 0L && is.finite(primary_eff$vol_reduction_pct[[1]])) scales::percent(primary_eff$vol_reduction_pct[[1]], accuracy = 1) else "N/A",
    ea_history_context_label(history_context),
    "neutral",

    "Basis Risk",
    if (nrow(primary_hedge) > 0L && is.finite(primary_hedge$r_squared[[1]])) scales::number(1 - primary_hedge$r_squared[[1]], accuracy = 0.01) else "N/A",
    "1 - R\u00b2",
    "neutral",

    "Ratio Stability",
    if (nrow(primary_stab) > 0L && is.finite(primary_stab$beta_sd[[1]])) scales::number(primary_stab$beta_sd[[1]], accuracy = 0.01) else "N/A",
    paste0(window, "d rolling"),
    "neutral",

    "Half-Life",
    if (nrow(primary_ou) > 0L && is.finite(primary_ou$half_life[[1]])) paste0(round(primary_ou$half_life[[1]]), "d") else "N/A",
    "context residual",
    "neutral"
  )

  list(
    available_markets = available_markets,
    history_context = history_context,
    history_context_label = ea_history_context_label(history_context),
    display_window = display_window,
    context_window = context_window,
    hedge_ratios = hedge_ratios,
    rolling_beta = rolling_beta,
    hedge_effectiveness = hedge_effectiveness,
    residual_timeseries = residual_timeseries,
    ou_fit = ou_fit,
    cross_hedge_matrix = cross_hedge_matrix,
    per_tenor_ratios = per_tenor_ratios,
    ratio_stability = ratio_stability,
    hedge_cost = hedge_cost,
    stress_period_comparison = stress_period_comparison,
    residual_with_bands = residual_with_bands,
    kpis = kpis,
    notes = c(
      "Rolling hedge-ratio charts use the selected display range, but the regression window is allowed to warm up before the visible start date.",
      paste0("Cross-hedge ranking, hedge effectiveness, and tenor ratios use the ", ea_history_context_label(history_context), " history window ending on the display-range end date."),
      "Residual bands come from an OU fit on price-level hedge residuals and use the stationary OU standard deviation."
    ),
    assumptions = c(
      paste0("Rolling window: ", window, " trading days."),
      "Roll cost is estimated from the latest available M1-M2 spread at the display-range end date.",
      "Stress regime = top quartile of rolling 20d benchmark volatility inside the selected history context."
    )
  )
}
