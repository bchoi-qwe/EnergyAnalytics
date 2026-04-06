# ---- Hedging Calculation Layer ----
# Hedge ratios, rolling betas, effectiveness, and mean reversion.

ea_calc_hedging <- function(filters) {
  catalog <- ea_market_catalog()
  markets <- ea_coalesce(filters$commodities, c("CL", "BRN", "RB"))
  benchmark <- ea_coalesce(filters$comparison_commodity, "CL")
  window <- ea_parse_rolling_window(ea_coalesce(filters$rolling_window, "63D"))
  annualize <- sqrt(252)


  # Hedge targets: markets that are NOT the benchmark
  targets <- setdiff(markets, benchmark)
  all_mkts <- unique(c(targets, benchmark))

  # Front contract log returns
  all_returns <- ea_load_dataset("commodity_curve_long") |>
    dplyr::filter(.data$market %in% all_mkts, .data$curve_point_num == 1) |>
    dplyr::arrange(.data$market, .data$date) |>
    dplyr::group_by(.data$market) |>
    dplyr::mutate(log_return = suppressWarnings(log(.data$value / dplyr::lag(.data$value)))) |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(.data$log_return))

  returns_wide <- all_returns |>
    dplyr::select("date", "market", "log_return") |>
    tidyr::pivot_wider(names_from = "market", values_from = "log_return") |>
    dplyr::arrange(.data$date) |>
    stats::na.omit()

  # --- hedge_ratios: OLS beta of each target vs benchmark ---
  hedge_ratios <- purrr::map_dfr(targets, function(mkt) {
    df <- returns_wide |> dplyr::select(y = dplyr::all_of(mkt), x = dplyr::all_of(benchmark))
    fit <- stats::lm(y ~ x, data = df)
    tidy_coef <- broom::tidy(fit)
    glance_fit <- broom::glance(fit)

    x_row <- tidy_coef[tidy_coef$term == "x", ]

    tibble::tibble(
      market = mkt,
      beta = x_row$estimate,
      r_squared = glance_fit$r.squared,
      std_error = x_row$std.error
    )
  })

  # --- rolling_beta: rolling OLS beta over time ---
  rolling_beta <- purrr::map_dfr(targets, function(mkt) {
    y_col <- returns_wide[[mkt]]
    x_col <- returns_wide[[benchmark]]

    roll_beta <- slider::slide2_dbl(
      y_col, x_col,
      ~ {
        if (length(.x) < 10) return(NA_real_)
        fit <- stats::lm(.x ~ .y)
        stats::coef(fit)[[2]]
      },
      .before = window - 1L, .complete = TRUE
    )

    roll_r2 <- slider::slide2_dbl(
      y_col, x_col,
      ~ {
        if (length(.x) < 10) return(NA_real_)
        fit <- stats::lm(.x ~ .y)
        broom::glance(fit)$r.squared
      },
      .before = window - 1L, .complete = TRUE
    )

    tibble::tibble(
      date = returns_wide$date,
      market = mkt,
      beta = roll_beta,
      r_squared = roll_r2
    ) |> dplyr::filter(!is.na(.data$beta))
  })

  # --- hedge_effectiveness: hedged vs unhedged vol ---
  hedge_effectiveness <- purrr::map_dfr(targets, function(mkt) {
    df <- returns_wide |> dplyr::select(y = dplyr::all_of(mkt), x = dplyr::all_of(benchmark))
    fit <- stats::lm(y ~ x, data = df)
    resid <- stats::residuals(fit)

    unhedged <- stats::sd(df$y, na.rm = TRUE) * annualize
    hedged <- stats::sd(resid, na.rm = TRUE) * annualize

    tibble::tibble(
      market = mkt,
      unhedged_vol = unhedged,
      hedged_vol = hedged,
      vol_reduction_pct = 1 - hedged / unhedged
    )
  })

  # --- residual_timeseries ---
  residual_timeseries <- purrr::map_dfr(targets, function(mkt) {
    df <- returns_wide |> dplyr::select("date", y = dplyr::all_of(mkt), x = dplyr::all_of(benchmark))
    fit <- stats::lm(y ~ x, data = df)

    tibble::tibble(
      date = df$date,
      market = mkt,
      residual = as.numeric(stats::residuals(fit))
    )
  })

  # --- ou_fit: OU parameters on hedge residuals ---
  ou_fit <- purrr::map_dfr(targets, function(mkt) {
    resid <- residual_timeseries |>
      dplyr::filter(.data$market == mkt) |>
      dplyr::pull(.data$residual)

    if (length(resid) < 30) {
      return(tibble::tibble(
        market = mkt, theta = NA_real_, mu = NA_real_,
        sigma = NA_real_, half_life = NA_real_
      ))
    }

    # cumulative residual for OU fitting (levels, not returns)
    cum_resid <- cumsum(resid)

    tryCatch({
      ou <- RTL::fitOU(spread = cum_resid)
      tibble::tibble(
        market = mkt, theta = ou$theta, mu = ou$mu,
        sigma = ou$sigma, half_life = ou$halfLife
      )
    }, error = function(e) {
      tibble::tibble(
        market = mkt, theta = NA_real_, mu = NA_real_,
        sigma = NA_real_, half_life = NA_real_
      )
    })
  })

  # --- cross_hedge_matrix: pairwise beta/R-squared ---
  all_pairs <- utils::combn(all_mkts, 2, simplify = FALSE)
  cross_hedge_matrix <- purrr::map_dfr(all_pairs, function(pr) {
    if (!all(pr %in% names(returns_wide))) {
      return(tibble::tibble(
        market_x = pr[1], market_y = pr[2], beta = NA_real_, r_squared = NA_real_
      ))
    }
    df <- returns_wide |> dplyr::select(x = dplyr::all_of(pr[1]), y = dplyr::all_of(pr[2]))
    fit <- stats::lm(y ~ x, data = df)
    tibble::tibble(
      market_x = pr[1], market_y = pr[2],
      beta = stats::coef(fit)[["x"]],
      r_squared = broom::glance(fit)$r.squared
    )
  })

  # --- per_tenor_ratios: OLS beta at each tenor ---
  per_tenor_ratios <- purrr::map_dfr(targets, function(mkt) {
    all_tenor_data <- ea_load_dataset("commodity_curve_long") |>
      dplyr::filter(.data$market %in% c(mkt, benchmark))

    tenor_nums <- sort(unique(all_tenor_data$curve_point_num))

    purrr::map_dfr(tenor_nums, function(tn) {
      wide <- all_tenor_data |>
        dplyr::filter(.data$curve_point_num == tn) |>
        dplyr::select("date", "market", "value") |>
        tidyr::pivot_wider(names_from = "market", values_from = "value") |>
        dplyr::arrange(.data$date) |>
        stats::na.omit()

      if (nrow(wide) < 30 || !all(c(mkt, benchmark) %in% names(wide))) {
        return(tibble::tibble(market = mkt, tenor = tn, beta = NA_real_, r_squared = NA_real_))
      }

      y_ret <- suppressWarnings(diff(log(wide[[mkt]])))
      x_ret <- suppressWarnings(diff(log(wide[[benchmark]])))
      valid <- !is.na(y_ret) & !is.na(x_ret) & is.finite(y_ret) & is.finite(x_ret)
      if (sum(valid) < 20) return(tibble::tibble(market = mkt, tenor = tn, beta = NA_real_, r_squared = NA_real_))

      fit <- stats::lm(y_ret[valid] ~ x_ret[valid])
      tibble::tibble(
        market = mkt, tenor = tn,
        beta = stats::coef(fit)[[2]],
        r_squared = broom::glance(fit)$r.squared
      )
    })
  })

  # --- ratio_stability: SD of rolling beta per target ---
  ratio_stability <- rolling_beta |>
    dplyr::group_by(.data$market) |>
    dplyr::summarise(beta_sd = stats::sd(.data$beta, na.rm = TRUE), .groups = "drop")

  # --- hedge_cost: roll yield of benchmark + residual basis risk ---
  hedge_cost <- purrr::map_dfr(targets, function(mkt) {
    eff <- hedge_effectiveness |> dplyr::filter(.data$market == mkt)

    bench_curves <- ea_load_dataset("commodity_curve_long") |>
      dplyr::filter(.data$market == benchmark, .data$curve_point_num %in% c(1L, 2L))

    bench_wide <- bench_curves |>
      dplyr::select("date", "curve_point_num", "value") |>
      tidyr::pivot_wider(names_from = "curve_point_num", values_from = "value") |>
      dplyr::filter(!is.na(.data$`1`), !is.na(.data$`2`)) |>
      dplyr::arrange(.data$date)

    if (nrow(bench_wide) == 0) {
      return(tibble::tibble(market = mkt, roll_cost = NA_real_, basis_risk_cost = NA_real_))
    }

    latest <- bench_wide |> dplyr::filter(.data$date == max(.data$date))
    roll_cost <- if (nrow(latest) > 0 && latest$`1`[1] > 0) {
      (latest$`1`[1] - latest$`2`[1]) / latest$`1`[1] * 12
    } else {
      NA_real_
    }

    tibble::tibble(
      market = mkt,
      roll_cost = roll_cost,
      basis_risk_cost = if (nrow(eff) > 0) eff$hedged_vol[1] else NA_real_
    )
  })

  # --- stress_period_comparison: normal vs high-vol period R-squared ---
  stress_period_comparison <- tryCatch({
    vol_series <- returns_wide[[benchmark]]
    roll_vol <- slider::slide_dbl(vol_series, ~ stats::sd(.x, na.rm = TRUE), .before = 19L, .complete = TRUE)
    is_stress <- roll_vol > stats::quantile(roll_vol, 0.75, na.rm = TRUE)

    purrr::map_dfr(targets, function(mkt) {
      if (!mkt %in% names(returns_wide)) return(tibble::tibble())

      normal_idx <- !is_stress & !is.na(returns_wide[[mkt]]) & !is.na(returns_wide[[benchmark]])
      stress_idx <- is_stress & !is.na(returns_wide[[mkt]]) & !is.na(returns_wide[[benchmark]])

      r2_normal <- if (sum(normal_idx, na.rm = TRUE) >= 20) {
        fit <- stats::lm(returns_wide[[mkt]][normal_idx] ~ returns_wide[[benchmark]][normal_idx])
        summary(fit)$r.squared
      } else NA_real_

      r2_stress <- if (sum(stress_idx, na.rm = TRUE) >= 20) {
        fit <- stats::lm(returns_wide[[mkt]][stress_idx] ~ returns_wide[[benchmark]][stress_idx])
        summary(fit)$r.squared
      } else NA_real_

      tibble::tibble(market = mkt, r2_normal = r2_normal, r2_stress = r2_stress)
    })
  }, error = function(e) {
    tibble::tibble(market = character(), r2_normal = numeric(), r2_stress = numeric())
  })

  # --- residual_with_bands: cumulative residual + OU bands ---
  residual_with_bands <- purrr::map_dfr(targets, function(mkt) {
    resid_df <- residual_timeseries |> dplyr::filter(.data$market == mkt)
    ou_params <- ou_fit |> dplyr::filter(.data$market == mkt)

    if (nrow(ou_params) == 0 || is.na(ou_params$mu[1])) {
      return(resid_df |> dplyr::mutate(ou_mu = NA_real_, band_1_lo = NA_real_, band_1_hi = NA_real_,
                                        band_2_lo = NA_real_, band_2_hi = NA_real_))
    }

    cum_resid <- cumsum(resid_df$residual)
    ou_mu  <- ou_params$mu[1]
    ou_sig <- ou_params$sigma[1]

    tibble::tibble(
      date = resid_df$date,
      market = mkt,
      residual = cum_resid,
      ou_mu = ou_mu,
      band_1_lo = ou_mu - ou_sig,
      band_1_hi = ou_mu + ou_sig,
      band_2_lo = ou_mu - 2 * ou_sig,
      band_2_hi = ou_mu + 2 * ou_sig
    )
  })

  # --- KPIs ---
  # Best proxy hedge by R-squared
  best_proxy <- cross_hedge_matrix |>
    dplyr::filter(.data$market_x != .data$market_y) |>
    dplyr::slice_max(.data$r_squared, n = 1, with_ties = FALSE)

  primary_hedge <- if (length(targets) > 0) hedge_ratios |> dplyr::filter(.data$market == targets[1]) else hedge_ratios[0, ]
  primary_eff   <- if (length(targets) > 0) hedge_effectiveness |> dplyr::filter(.data$market == targets[1]) else hedge_effectiveness[0, ]
  primary_ou    <- if (length(targets) > 0) ou_fit |> dplyr::filter(.data$market == targets[1]) else ou_fit[0, ]
  primary_stab  <- if (length(targets) > 0) ratio_stability |> dplyr::filter(.data$market == targets[1]) else ratio_stability[0, ]

  kpis <- tibble::tribble(
    ~title, ~value, ~delta, ~status,
    "Best Proxy",
    if (nrow(best_proxy) > 0) paste(best_proxy$market_x[1], "vs", best_proxy$market_y[1]) else "N/A",
    if (nrow(best_proxy) > 0) paste0("R\u00b2=", scales::number(best_proxy$r_squared[1], accuracy = 0.01)) else "",
    "positive",

    "Hedge Ratio",
    if (nrow(primary_hedge) > 0) scales::number(primary_hedge$beta[1], accuracy = 0.01) else "N/A",
    if (length(targets) > 0) paste0("vs ", benchmark) else "",
    "neutral",

    "Effectiveness",
    if (nrow(primary_eff) > 0) scales::percent(primary_eff$vol_reduction_pct[1], accuracy = 1) else "N/A",
    "vol reduction",
    if (nrow(primary_eff) > 0 && primary_eff$vol_reduction_pct[1] > 0.5) "positive" else "warning",

    "Basis Risk",
    if (nrow(primary_hedge) > 0) scales::number(1 - primary_hedge$r_squared[1], accuracy = 0.01) else "N/A",
    "1 - R\u00b2",
    if (nrow(primary_hedge) > 0 && (1 - primary_hedge$r_squared[1]) > 0.3) "warning" else "neutral",

    "Ratio Stability",
    if (nrow(primary_stab) > 0) scales::number(primary_stab$beta_sd[1], accuracy = 0.01) else "N/A",
    "rolling beta SD",
    if (nrow(primary_stab) > 0 && primary_stab$beta_sd[1] > 0.15) "warning" else "neutral",

    "Half-Life",
    if (nrow(primary_ou) > 0 && !is.na(primary_ou$half_life[1])) paste0(round(primary_ou$half_life[1]), "d") else "N/A",
    "OU mean-rev",
    if (nrow(primary_ou) > 0 && !is.na(primary_ou$half_life[1]) && primary_ou$half_life[1] < 30) "positive" else "neutral"
  )

  notes <- c(
    "Hedge ratios computed via OLS regression of front-month log returns.",
    "Rolling beta uses a sliding window; confidence bands reflect \u00b1 1 std error.",
    "OU mean-reversion fit via RTL::fitOU on cumulative hedge residuals."
  )
  assumptions <- c(
    paste0("Rolling window: ", window, " trading days."),
    "Roll cost estimated from benchmark M1-M2 spread annualised at 12x.",
    "Stress period = top quartile of rolling 20d benchmark volatility."
  )

  list(
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
    notes = notes,
    assumptions = assumptions
  )
}
