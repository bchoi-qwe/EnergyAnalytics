# ---- Scenarios Calculation Layer ----
# Monte Carlo simulation, stress testing, VaR, and correlation stress.

ea_calc_scenarios <- function(filters, shocks = list(flat = 0, vol = 0, spread = 0)) {
  catalog <- ea_market_catalog()
  defaults <- ea_global_filter_defaults(catalog)
  markets <- ea_coalesce(filters$commodities, defaults$commodities)
  expiry_lo <- ea_coalesce(filters$expiry_range[1], 1L)
  expiry_hi <- ea_coalesce(filters$expiry_range[2], 12L)
  annualize <- sqrt(252)

  curves <- tryCatch(ea_load_dataset("commodity_curve_long"), error = function(e) NULL)

  if (is.null(curves)) {
    return(list(
      price_simulations = tibble::tibble(t = numeric(), market = character(), sim_id = character(), price = numeric()),
      ou_simulations = tibble::tibble(t = numeric(), spread_label = character(), sim_id = character(), value = numeric()),
      stress_scenarios = tibble::tibble(scenario_label = character(), market = character(), shock_pct = numeric(), new_price = numeric(), pnl_impact = numeric()),
      spread_option_pnl = tibble::tibble(spread_level = numeric(), payoff = numeric(), spread_label = character()),
      var_summary = tibble::tibble(market = character(), var_95 = numeric(), var_99 = numeric(), cvar_95 = numeric(), cvar_99 = numeric()),
      correlation_stress = tibble::tibble(market_x = character(), market_y = character(), corr_normal = numeric(), corr_stress = numeric())
    ))
  }

  curves <- curves |>
    dplyr::filter(
      .data$market %in% markets,
      .data$curve_point_num >= expiry_lo,
      .data$curve_point_num <= expiry_hi
    )

  front <- curves |>
    dplyr::filter(.data$curve_point_num == 1) |>
    dplyr::arrange(.data$market, .data$date)

  latest_prices <- front |>
    dplyr::group_by(.data$market) |>
    dplyr::summarise(price = dplyr::last(.data$value), .groups = "drop")

  front_returns <- front |>
    dplyr::filter(.data$value > 0) |>
    dplyr::group_by(.data$market) |>
    dplyr::mutate(log_return = log(.data$value / dplyr::lag(.data$value))) |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(.data$log_return), is.finite(.data$log_return))

  nsims <- 100L
  horizon <- 63L

  price_simulations <- purrr::map_dfr(markets, function(mkt) {
    mkt_returns <- front_returns |> dplyr::filter(.data$market == mkt)
    if (nrow(mkt_returns) < 10L) {
      return(tibble::tibble(t = numeric(), market = character(), sim_id = character(), price = numeric()))
    }

    s0 <- latest_prices$price[latest_prices$market == mkt]
    if (length(s0) == 0L || is.na(s0)) {
      return(tibble::tibble(t = numeric(), market = character(), sim_id = character(), price = numeric()))
    }

    mu <- mean(mkt_returns$log_return, na.rm = TRUE) * 252
    sigma <- stats::sd(mkt_returns$log_return, na.rm = TRUE) * annualize

    tryCatch({
      RTL::simGBM(
        nsims = nsims,
        S0 = s0,
        drift = mu,
        sigma = sigma,
        T2M = horizon / 252,
        dt = 1 / 252
      ) |>
        tidyr::pivot_longer(cols = -"t", names_to = "sim_id", values_to = "price") |>
        dplyr::mutate(market = mkt)
    }, error = function(e) {
      tibble::tibble(t = numeric(), market = character(), sim_id = character(), price = numeric())
    })
  })

  spread_pairs <- list("CL-BRN" = c("CL", "BRN"))
  available_pairs <- purrr::keep(spread_pairs, ~ all(.x %in% markets))

  ou_simulations <- purrr::map_dfr(names(available_pairs), function(sp_lbl) {
    legs <- available_pairs[[sp_lbl]]
    spread_ts <- front |>
      dplyr::filter(.data$market %in% legs) |>
      dplyr::select("date", "market", "value") |>
      tidyr::pivot_wider(names_from = "market", values_from = "value") |>
      stats::na.omit()

    if (nrow(spread_ts) < 30L) {
      return(tibble::tibble(t = numeric(), spread_label = character(), sim_id = character(), value = numeric()))
    }

    spread_vals <- spread_ts[[legs[1]]] - spread_ts[[legs[2]]]
    s0 <- spread_vals[length(spread_vals)]

    tryCatch({
      ou_params <- RTL::fitOU(spread = spread_vals)
      RTL::simOU(
        nsims = nsims,
        S0 = s0,
        mu = ou_params$mu,
        theta = ou_params$theta,
        sigma = ou_params$sigma,
        T2M = horizon / 252,
        dt = 1 / 252
      ) |>
        tidyr::pivot_longer(cols = -"t", names_to = "sim_id", values_to = "value") |>
        dplyr::mutate(spread_label = sp_lbl)
    }, error = function(e) {
      tibble::tibble(t = numeric(), spread_label = character(), sim_id = character(), value = numeric())
    })
  })

  scenarios <- tibble::tribble(
    ~scenario_label, ~shock_pct,
    "Supply squeeze (+15%)", 0.15,
    "Demand collapse (-20%)", -0.20,
    "Vol spike (flat price)", 0.0,
    "Custom shock", ea_coalesce(shocks$flat, 0) / 100
  )

  stress_scenarios <- purrr::map_dfr(seq_len(nrow(scenarios)), function(i) {
    sc <- scenarios[i, ]
    latest_prices |>
      dplyr::mutate(
        scenario_label = sc$scenario_label,
        shock_pct = sc$shock_pct,
        new_price = .data$price * (1 + sc$shock_pct),
        pnl_impact = .data$new_price - .data$price
      )
  })

  empty_spread <- tibble::tibble(spread_level = numeric(), payoff = numeric(), spread_label = character())

  spread_option_pnl <- if (length(markets) < 2L) {
    empty_spread
  } else {
    tryCatch({
      m1 <- markets[1]
      m2 <- markets[2]
      f1 <- latest_prices$price[latest_prices$market == m1]
      f2 <- latest_prices$price[latest_prices$market == m2]
      if (length(f1) == 0L || length(f2) == 0L) stop("missing prices")

      sig1 <- stats::sd(front_returns$log_return[front_returns$market == m1], na.rm = TRUE) * annualize
      sig2 <- stats::sd(front_returns$log_return[front_returns$market == m2], na.rm = TRUE) * annualize

      returns_wide <- front_returns |>
        dplyr::select("date", "market", "log_return") |>
        tidyr::pivot_wider(names_from = "market", values_from = "log_return") |>
        stats::na.omit()

      rho <- stats::cor(returns_wide[[m1]], returns_wide[[m2]], use = "pairwise.complete.obs")
      current_spread <- f1 - f2
      spread_range <- seq(current_spread * 0.5, current_spread * 1.5, length.out = 20)

      purrr::map_dfr(spread_range, function(x) {
        result <- tryCatch(
          RTL::spreadOption(
            F1 = f1,
            F2 = f2,
            X = x,
            sigma1 = sig1,
            sigma2 = sig2,
            rho = rho,
            T2M = 0.25,
            r = 0.045
          ),
          error = function(e) list(price = NA_real_)
        )

        tibble::tibble(
          spread_level = x,
          payoff = result$price,
          spread_label = paste(m1, "vs", m2)
        )
      })
    }, error = function(e) {
      empty_spread
    })
  }

  var_summary <- front_returns |>
    dplyr::group_by(.data$market) |>
    dplyr::summarise(
      var_95 = stats::quantile(.data$log_return, 0.05, na.rm = TRUE),
      var_99 = stats::quantile(.data$log_return, 0.01, na.rm = TRUE),
      cvar_95 = mean(.data$log_return[.data$log_return <= stats::quantile(.data$log_return, 0.05, na.rm = TRUE)], na.rm = TRUE),
      cvar_99 = mean(.data$log_return[.data$log_return <= stats::quantile(.data$log_return, 0.01, na.rm = TRUE)], na.rm = TRUE),
      .groups = "drop"
    )

  returns_wide_all <- front_returns |>
    dplyr::select("date", "market", "log_return") |>
    tidyr::pivot_wider(names_from = "market", values_from = "log_return") |>
    dplyr::arrange(.data$date) |>
    stats::na.omit()

  market_cols <- setdiff(names(returns_wide_all), "date")

  if (length(market_cols) >= 2L && nrow(returns_wide_all) >= 60L) {
    ret_mat <- as.matrix(returns_wide_all[, market_cols])
    avg_abs <- rowMeans(abs(ret_mat))
    vol_threshold <- stats::quantile(avg_abs, 0.75)
    normal_idx <- avg_abs <= vol_threshold
    stress_idx <- avg_abs > vol_threshold
    corr_normal <- stats::cor(ret_mat[normal_idx, , drop = FALSE], use = "pairwise.complete.obs")
    corr_stress_mat <- stats::cor(ret_mat[stress_idx, , drop = FALSE], use = "pairwise.complete.obs")

    pairs <- expand.grid(market_x = market_cols, market_y = market_cols, stringsAsFactors = FALSE)
    correlation_stress <- tibble::as_tibble(pairs) |>
      dplyr::rowwise() |>
      dplyr::mutate(
        corr_normal = corr_normal[.data$market_x, .data$market_y],
        corr_stress = corr_stress_mat[.data$market_x, .data$market_y]
      ) |>
      dplyr::ungroup()
  } else {
    correlation_stress <- tibble::tibble(
      market_x = character(),
      market_y = character(),
      corr_normal = numeric(),
      corr_stress = numeric()
    )
  }

  historical_analog <- tryCatch({
    primary <- markets[1]
    mkt_ret <- front_returns |> dplyr::filter(.data$market == primary)
    if (nrow(mkt_ret) < 252L) stop("insufficient")

    recent_window <- utils::tail(mkt_ret$log_return, 63)
    n <- length(mkt_ret$log_return)
    if (n < 189L) stop("insufficient history")

    scores <- purrr::map_dbl(seq_len(n - 126L), function(i) {
      hist_window <- mkt_ret$log_return[i:(i + 62L)]
      stats::cor(recent_window, hist_window, use = "pairwise.complete.obs")
    })
    scores[is.na(scores)] <- -Inf

    top3 <- order(scores, decreasing = TRUE)[seq_len(min(3L, length(scores)))]

    purrr::map_dfr(seq_along(top3), function(j) {
      i <- top3[j]
      fwd_start <- i + 63L
      fwd_end <- min(fwd_start + 62L, n)
      if (fwd_start > n) return(tibble::tibble())

      fwd_rets <- mkt_ret$log_return[fwd_start:fwd_end]
      cum_ret <- c(0, cumsum(fwd_rets))
      tibble::tibble(
        analog_id = paste0("Analog ", j),
        day = seq_along(cum_ret) - 1L,
        cumulative_return = cum_ret,
        match_date = mkt_ret$date[i],
        match_corr = scores[top3[j]]
      )
    })
  }, error = function(e) {
    tibble::tibble(
      analog_id = character(),
      day = integer(),
      cumulative_return = numeric(),
      match_date = as.Date(character()),
      match_corr = numeric()
    )
  })

  factor_decomposition <- tryCatch({
    if (nrow(stress_scenarios) == 0L) stop("no stress scenarios")

    stress_scenarios |>
      dplyr::group_by(.data$scenario_label) |>
      dplyr::summarise(
        flat_contribution = mean(.data$pnl_impact * (.data$shock_pct != 0), na.rm = TRUE),
        vol_contribution = 0,
        spread_contribution = 0,
        total_pnl = sum(.data$pnl_impact, na.rm = TRUE),
        .groups = "drop"
      ) |>
      tidyr::pivot_longer(
        cols = c("flat_contribution", "vol_contribution", "spread_contribution"),
        names_to = "factor",
        values_to = "contribution"
      ) |>
      dplyr::mutate(factor = dplyr::case_when(
        .data$factor == "flat_contribution" ~ "Flat Price",
        .data$factor == "vol_contribution" ~ "Volatility",
        .data$factor == "spread_contribution" ~ "Spread",
        TRUE ~ .data$factor
      ))
  }, error = function(e) {
    tibble::tibble(scenario_label = character(), factor = character(), contribution = numeric(), total_pnl = numeric())
  })

  return_distribution <- tryCatch({
    if (nrow(price_simulations) == 0L) stop("no sims")

    price_simulations |>
      dplyr::group_by(.data$market, .data$sim_id) |>
      dplyr::summarise(
        terminal_price = utils::tail(.data$price, 1L),
        initial_price = utils::head(.data$price, 1L),
        .groups = "drop"
      ) |>
      dplyr::filter(.data$initial_price > 0) |>
      dplyr::mutate(terminal_return = log(.data$terminal_price / .data$initial_price)) |>
      dplyr::group_by(.data$market) |>
      dplyr::summarise(
        returns = list(.data$terminal_return),
        mean_return = mean(.data$terminal_return, na.rm = TRUE),
        sd_return = stats::sd(.data$terminal_return, na.rm = TRUE),
        var_95 = stats::quantile(.data$terminal_return, 0.05, na.rm = TRUE),
        .groups = "drop"
      )
  }, error = function(e) {
    tibble::tibble(
      market = character(),
      returns = list(),
      mean_return = numeric(),
      sd_return = numeric(),
      var_95 = numeric()
    )
  })

  impact_curve <- tryCatch({
    primary <- markets[1]
    flat_shock <- ea_coalesce(shocks$flat, 0)
    curves |>
      dplyr::filter(.data$market == primary, .data$date == max(.data$date)) |>
      dplyr::arrange(.data$curve_point_num) |>
      dplyr::select(tenor = .data$curve_point_num, base = .data$value) |>
      dplyr::mutate(impact = .data$base * (1 + flat_shock / 100))
  }, error = function(e) {
    tibble::tibble(tenor = integer(), base = numeric(), impact = numeric())
  })

  propagation <- tryCatch({
    flat_shock_val <- ea_coalesce(shocks$flat, 0)
    vol_shock_val <- ea_coalesce(shocks$vol, 0)
    spread_shock_val <- ea_coalesce(shocks$spread, 0)
    primary <- markets[1]
    s0 <- latest_prices$price[latest_prices$market == primary]
    s0 <- if (length(s0) > 0L) s0[1] else 0

    tibble::tibble(
      factor = c("Flat Price", "Volatility", "Spread"),
      contribution = c(
        s0 * flat_shock_val / 100,
        vol_shock_val * 0.1,
        spread_shock_val * 0.5
      )
    )
  }, error = function(e) {
    tibble::tibble(factor = character(), contribution = numeric())
  })

  presets <- tibble::tribble(
    ~id, ~title, ~description, ~flat, ~vol, ~spread,
    "supply_squeeze", "Supply Squeeze", "Sudden supply disruption with a bullish flat-price shock.", 15, 5, 2,
    "demand_collapse", "Demand Collapse", "Economic recession scenario with bearish flat price.", -20, 8, -3,
    "vol_spike", "Vol Spike", "Heightened uncertainty with a volatility shock only.", 0, 15, 0,
    "treasury_bear", "Treasury Bear Steepener", "Rising long rates with mild price pressure and wider spreads.", -5, 3, 2,
    "opec_action", "OPEC Action", "Supply cut with a strong bullish flat-price shock.", 20, 4, 1,
    "demand_recession", "Demand Recession", "Broad commodity selloff with wider downside risk.", -25, 10, -5
  )

  primary_var <- var_summary |> dplyr::filter(.data$market == markets[1])

  kpis <- tibble::tribble(
    ~title, ~value, ~delta, ~status,
    "95% VaR",
    if (nrow(primary_var) > 0 && !is.na(primary_var$var_95[1])) scales::percent(primary_var$var_95[1], accuracy = 0.01) else "N/A",
    "1-day loss",
    if (nrow(primary_var) > 0 && !is.na(primary_var$var_95[1]) && primary_var$var_95[1] < -0.03) "warning" else "neutral",
    "99% VaR",
    if (nrow(primary_var) > 0 && !is.na(primary_var$var_99[1])) scales::percent(primary_var$var_99[1], accuracy = 0.01) else "N/A",
    "1-day loss",
    if (nrow(primary_var) > 0 && !is.na(primary_var$var_99[1]) && primary_var$var_99[1] < -0.05) "warning" else "neutral",
    "CVaR 95%",
    if (nrow(primary_var) > 0 && !is.na(primary_var$cvar_95[1])) scales::percent(primary_var$cvar_95[1], accuracy = 0.01) else "N/A",
    "expected tail loss",
    if (nrow(primary_var) > 0 && !is.na(primary_var$cvar_95[1]) && primary_var$cvar_95[1] < -0.04) "warning" else "neutral",
    "Sim Paths",
    if (nrow(price_simulations) > 0L) scales::comma(length(unique(price_simulations$sim_id))) else "0",
    "GBM Monte Carlo",
    "neutral",
    "Best Analog",
    if (nrow(historical_analog) > 0 && !is.na(historical_analog$match_date[1])) format(historical_analog$match_date[1], "%b %Y") else "N/A",
    if (nrow(historical_analog) > 0 && !is.na(historical_analog$match_corr[1])) paste0("corr=", round(historical_analog$match_corr[1], 2)) else "",
    "positive"
  )

  notes <- c(
    "GBM Monte Carlo uses historical drift and realized volatility for calibration.",
    "Historical analog matching compares the recent 63-day window against prior 63-day windows by Pearson correlation.",
    "VaR and CVaR are computed from daily log returns using historical simulation."
  )

  assumptions <- c(
    "Monte Carlo uses 100 simulations over a 63-day horizon.",
    "Analog matching uses the focused market, which is passed first in the market set.",
    "Stress scenarios currently model flat shocks only; vol and spread interactions are simplified."
  )

  list(
    price_simulations = price_simulations,
    ou_simulations = ou_simulations,
    stress_scenarios = stress_scenarios,
    spread_option_pnl = spread_option_pnl,
    var_summary = var_summary,
    correlation_stress = correlation_stress,
    historical_analog = historical_analog,
    factor_decomposition = factor_decomposition,
    return_distribution = return_distribution,
    impact_curve = impact_curve,
    propagation = propagation,
    presets = presets,
    kpis = kpis,
    notes = notes,
    assumptions = assumptions
  )
}
