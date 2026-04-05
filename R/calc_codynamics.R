# ---- Co-Dynamics Calculation Layer ----
# Cross-market correlations, spreads, betas, and PCA.

ea_parse_rolling_window <- function(window_str) {
  n <- as.integer(gsub("[^0-9]", "", window_str))
  if (is.na(n)) 63L else n
}

ea_calc_codynamics <- function(filters) {
  catalog <- ea_market_catalog()
  curves <- ea_load_filtered_curves(filters)
  markets <- unique(curves$market)
  comparison <- ea_coalesce(filters$comparison_commodity, "NG")
  focus_markets <- unique(c(markets, comparison))
  window <- ea_parse_rolling_window(ea_coalesce(filters$rolling_window, "63D"))

  # Front contract returns for all focus markets
  all_curves <- ea_load_dataset("commodity_curve_long") |>
    dplyr::filter(.data$market %in% focus_markets, .data$curve_point_num == 1) |>
    dplyr::arrange(.data$market, .data$date) |>
    dplyr::group_by(.data$market) |>
    dplyr::mutate(log_return = log(.data$value / dplyr::lag(.data$value))) |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(.data$log_return))

  # Wide returns matrix
  returns_wide <- all_curves |>
    dplyr::select(.data$date, .data$market, .data$log_return) |>
    tidyr::pivot_wider(names_from = .data$market, values_from = .data$log_return) |>
    dplyr::arrange(.data$date) |>
    stats::na.omit()

  market_cols <- setdiff(names(returns_wide), "date")
  returns_mat <- as.matrix(returns_wide[, market_cols])

  # --- correlation_matrix ---
  if (nrow(returns_mat) >= window) {
    tail_mat <- utils::tail(returns_mat, window)
    corr_mat <- stats::cor(tail_mat, use = "pairwise.complete.obs")
  } else {
    corr_mat <- stats::cor(returns_mat, use = "pairwise.complete.obs")
  }

  correlation_matrix <- expand.grid(
    market_x = market_cols, market_y = market_cols, stringsAsFactors = FALSE
  ) |>
    tibble::as_tibble() |>
    dplyr::rowwise() |>
    dplyr::mutate(correlation = corr_mat[.data$market_x, .data$market_y]) |>
    dplyr::ungroup()

  # --- correlation_timeseries ---
  pairs <- utils::combn(market_cols, 2, simplify = FALSE)

  correlation_timeseries <- purrr::map_dfr(pairs, function(pr) {
    x <- returns_wide[[pr[1]]]
    y <- returns_wide[[pr[2]]]

    roll_corr <- slider::slide2_dbl(
      x, y,
      ~ stats::cor(.x, .y, use = "pairwise.complete.obs"),
      .before = window - 1L, .complete = TRUE
    )

    tibble::tibble(
      date = returns_wide$date,
      pair = paste(pr[1], "vs", pr[2]),
      correlation = roll_corr,
      window = paste0(window, "d")
    ) |> dplyr::filter(!is.na(.data$correlation))
  })

  # --- spread_timeseries ---
  front_prices <- all_curves |>
    dplyr::select(.data$date, .data$market, .data$value)

  spread_definitions <- list(
    "CL-BRN" = c("CL", "BRN"),
    "CL-RB (Crack)" = c("RB", "CL"),
    "CL-HO (Crack)" = c("HO", "CL")
  )

  spread_timeseries <- purrr::map_dfr(names(spread_definitions), function(sp_lbl) {
    legs <- spread_definitions[[sp_lbl]]
    if (!all(legs %in% focus_markets)) return(tibble::tibble())

    wide_sp <- front_prices |>
      dplyr::filter(.data$market %in% legs) |>
      tidyr::pivot_wider(names_from = .data$market, values_from = .data$value) |>
      dplyr::filter(!is.na(!!dplyr::sym(legs[1])), !is.na(!!dplyr::sym(legs[2])))

    if (nrow(wide_sp) == 0L) return(tibble::tibble())

    tibble::tibble(
      date = wide_sp$date,
      spread_label = sp_lbl,
      value = wide_sp[[legs[1]]] - wide_sp[[legs[2]]]
    )
  })

  # --- spread_zscore ---
  spread_zscore <- spread_timeseries |>
    dplyr::group_by(.data$spread_label) |>
    dplyr::summarise(
      current = dplyr::last(stats::na.omit(.data$value)),
      mean = mean(.data$value, na.rm = TRUE),
      std = stats::sd(.data$value, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(zscore = (.data$current - .data$mean) / .data$std)

  # --- beta_matrix ---
  beta_matrix <- purrr::map_dfr(pairs, function(pr) {
    df <- returns_wide |>
      dplyr::select(dplyr::all_of(c("date", pr[1], pr[2]))) |>
      stats::na.omit()

    if (nrow(df) < 30) {
      return(tibble::tibble(market_x = pr[1], market_y = pr[2], beta = NA_real_, r_squared = NA_real_))
    }

    fit <- stats::lm(df[[pr[2]]] ~ df[[pr[1]]])
    tidy_fit <- broom::glance(fit)

    tibble::tibble(
      market_x = pr[1], market_y = pr[2],
      beta = stats::coef(fit)[[2]],
      r_squared = tidy_fit$r.squared
    )
  })

  # --- pca_decomposition ---
  pca_decomposition <- tryCatch({
    pca <- stats::prcomp(returns_mat, center = TRUE, scale. = TRUE)
    loadings <- as.data.frame(pca$rotation[, 1:min(3, ncol(pca$rotation))])
    n_pcs <- ncol(loadings)
    colnames(loadings) <- paste0("PC", seq_len(n_pcs))
    loadings$market <- rownames(loadings)

    for (pc in c("PC1", "PC2", "PC3")) {
      if (!pc %in% names(loadings)) loadings[[pc]] <- NA_real_
    }

    tibble::as_tibble(loadings) |>
      dplyr::select("market", "PC1", "PC2", "PC3")
  }, error = function(e) {
    tibble::tibble(market = market_cols, PC1 = NA_real_, PC2 = NA_real_, PC3 = NA_real_)
  })

  # Add SE bands to correlation_timeseries
  window_n <- window  # numeric scalar; avoids name clash with the 'window' column
  correlation_timeseries <- correlation_timeseries |>
    dplyr::mutate(
      se = sqrt((1 - .data$correlation^2) / pmax(window_n - 2, 1)),
      ci_lo = pmax(.data$correlation - 2 * .data$se, -1),
      ci_hi = pmin(.data$correlation + 2 * .data$se, 1)
    )

  treasury_betas <- tryCatch({
    ust <- ea_load_dataset("ust_curve_long")
    ust_wide <- ust |>
      dplyr::filter(.data$curve_point_num %in% c(2, 5, 10, 30)) |>
      dplyr::select("date", "curve_point_num", "value") |>
      tidyr::pivot_wider(names_from = "curve_point_num", values_from = "value") |>
      dplyr::arrange(.data$date)

    # Get column names that match 2, 5, 10, 30 (may be stored as "2", "5", "10", "30" or as integers)
    col_names <- setdiff(names(ust_wide), "date")
    col_2  <- col_names[grepl("^2$", col_names)][1]
    col_5  <- col_names[grepl("^5$", col_names)][1]
    col_10 <- col_names[grepl("^10$", col_names)][1]
    col_30 <- col_names[grepl("^30$", col_names)][1]

    if (any(is.na(c(col_2, col_5, col_10, col_30)))) stop("missing UST tenors")

    ust_factors <- ust_wide |>
      dplyr::mutate(
        level = (.data[[col_2]] + .data[[col_5]] + .data[[col_10]] + .data[[col_30]]) / 4,
        slope = .data[[col_10]] - .data[[col_2]],
        curvature = .data[[col_30]] - .data[[col_2]] - 2 * (.data[[col_10]] - .data[[col_2]])
      ) |>
      dplyr::arrange(.data$date) |>
      dplyr::mutate(
        level_chg    = .data$level    - dplyr::lag(.data$level),
        slope_chg    = .data$slope    - dplyr::lag(.data$slope),
        curvature_chg = .data$curvature - dplyr::lag(.data$curvature)
      ) |>
      dplyr::filter(!is.na(.data$level_chg)) |>
      dplyr::select("date", "level_chg", "slope_chg", "curvature_chg")

    purrr::map_dfr(focus_markets, function(mkt) {
      mkt_ret <- all_curves |>
        dplyr::filter(.data$market == mkt) |>
        dplyr::select("date", "log_return")

      joined <- dplyr::inner_join(mkt_ret, ust_factors, by = "date") |> stats::na.omit()
      if (nrow(joined) < 30) {
        return(tibble::tibble(market = mkt, factor = c("Level", "Slope", "Curvature"), beta = NA_real_))
      }

      fit <- stats::lm(log_return ~ level_chg + slope_chg + curvature_chg, data = joined)
      coefs <- stats::coef(fit)

      tibble::tibble(
        market = mkt,
        factor = c("Level", "Slope", "Curvature"),
        beta = as.numeric(coefs[c("level_chg", "slope_chg", "curvature_chg")])
      )
    })
  }, error = function(e) {
    tibble::tibble(market = character(), factor = character(), beta = numeric())
  })

  connectedness_score <- if (length(market_cols) >= 2) {
    mean(abs(corr_mat[upper.tri(corr_mat)]), na.rm = TRUE)
  } else {
    NA_real_
  }

  correlation_breaks <- correlation_timeseries |>
    dplyr::group_by(.data$pair) |>
    dplyr::mutate(
      lagged_corr = dplyr::lag(.data$correlation, 20L),
      corr_delta  = .data$correlation - .data$lagged_corr
    ) |>
    dplyr::filter(!is.na(.data$corr_delta)) |>
    dplyr::slice_max(.data$date, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::select("pair", "corr_delta")

  coint_residual <- tryCatch({
    pair_combos <- utils::combn(focus_markets, 2, simplify = FALSE)
    purrr::map_dfr(pair_combos, function(pr) {
      prices_wide <- all_curves |>
        dplyr::filter(.data$market %in% pr) |>
        dplyr::select("date", "market", "value") |>
        tidyr::pivot_wider(names_from = "market", values_from = "value") |>
        dplyr::arrange(.data$date) |>
        stats::na.omit()

      if (nrow(prices_wide) < 60 || !all(pr %in% names(prices_wide))) return(tibble::tibble())

      fit <- stats::lm(prices_wide[[pr[2]]] ~ prices_wide[[pr[1]]])
      resid_vec <- stats::residuals(fit)

      ou <- tryCatch(RTL::fitOU(spread = resid_vec), error = function(e) NULL)
      if (is.null(ou)) return(tibble::tibble())

      tibble::tibble(
        date = prices_wide$date,
        pair = paste(pr[1], "vs", pr[2]),
        residual = resid_vec,
        ou_mu = ou$mu,
        ou_sigma = ou$sigma,
        band_1_lo = ou$mu - ou$sigma,
        band_1_hi = ou$mu + ou$sigma,
        band_2_lo = ou$mu - 2 * ou$sigma,
        band_2_hi = ou$mu + 2 * ou$sigma
      )
    })
  }, error = function(e) {
    tibble::tibble(date = as.Date(character()), pair = character(), residual = numeric(),
                   ou_mu = numeric(), ou_sigma = numeric(),
                   band_1_lo = numeric(), band_1_hi = numeric(),
                   band_2_lo = numeric(), band_2_hi = numeric())
  })

  rolling_beta_ts <- purrr::map_dfr(pairs, function(pr) {
    x <- returns_wide[[pr[1]]]
    y <- returns_wide[[pr[2]]]
    roll_beta <- slider::slide2_dbl(
      y, x,
      ~ { if (length(.x) < 10) return(NA_real_); stats::coef(stats::lm(.x ~ .y))[[2]] },
      .before = window - 1L, .complete = TRUE
    )
    tibble::tibble(date = returns_wide$date, pair = paste(pr[1], "vs", pr[2]), beta = roll_beta) |>
      dplyr::filter(!is.na(.data$beta))
  })

  # Compute KPIs
  primary <- focus_markets[1]

  # Biggest correlation change
  biggest_break <- if (nrow(correlation_breaks) > 0) {
    correlation_breaks |>
      dplyr::slice_max(abs(.data$corr_delta), n = 1, with_ties = FALSE)
  } else {
    tibble::tibble(pair = "N/A", corr_delta = NA_real_)
  }

  # Spread with highest abs z-score
  top_spread <- if (nrow(spread_zscore) > 0) {
    spread_zscore |> dplyr::slice_max(abs(.data$zscore), n = 1)
  } else {
    tibble::tibble(spread_label = "N/A", zscore = NA_real_)
  }

  # CL-BRN correlation
  cl_brn_corr <- correlation_matrix |>
    dplyr::filter(.data$market_x == "CL", .data$market_y == "BRN") |>
    dplyr::pull(.data$correlation)
  cl_brn_corr <- if (length(cl_brn_corr) > 0) cl_brn_corr[1] else NA_real_

  # CL-NG correlation
  cl_ng_corr <- correlation_matrix |>
    dplyr::filter((.data$market_x == "CL" & .data$market_y == "NG") |
                  (.data$market_x == "NG" & .data$market_y == "CL")) |>
    dplyr::pull(.data$correlation)
  cl_ng_corr <- if (length(cl_ng_corr) > 0) cl_ng_corr[1] else NA_real_

  # CL-RB crack spread
  cl_rb_spread <- spread_zscore |>
    dplyr::filter(grepl("RB", .data$spread_label) | grepl("Crack", .data$spread_label)) |>
    dplyr::slice(1)
  crack_val <- if (nrow(cl_rb_spread) > 0) cl_rb_spread$current[1] else NA_real_

  kpis <- tibble::tribble(
    ~title, ~value, ~delta, ~status,
    "CL-BRN Corr",
    if (!is.na(cl_brn_corr)) scales::number(cl_brn_corr, accuracy = 0.01) else "N/A",
    paste0(window, "d rolling"),
    if (!is.na(cl_brn_corr) && cl_brn_corr > 0.8) "positive" else if (!is.na(cl_brn_corr) && cl_brn_corr < 0.5) "warning" else "neutral",

    "Crack Spread",
    if (!is.na(crack_val)) scales::number(crack_val, accuracy = 0.01) else "N/A",
    "CL-RB", "neutral",

    "CL-NG Corr",
    if (!is.na(cl_ng_corr)) scales::number(cl_ng_corr, accuracy = 0.01) else "N/A",
    paste0(window, "d rolling"),
    if (!is.na(cl_ng_corr) && abs(cl_ng_corr) < 0.3) "warning" else "neutral",

    "Connectedness",
    if (!is.na(connectedness_score)) scales::number(connectedness_score, accuracy = 0.01) else "N/A",
    "mean abs corr", "neutral",

    "Largest Corr Break",
    if (!is.na(biggest_break$corr_delta[1])) scales::number(biggest_break$corr_delta[1], accuracy = 0.01) else "N/A",
    if (nrow(biggest_break) > 0) biggest_break$pair[1] else "",
    if (!is.na(biggest_break$corr_delta[1]) && abs(biggest_break$corr_delta[1]) > 0.2) "warning" else "neutral",

    "Spread Outlier",
    if (!is.na(top_spread$zscore[1])) scales::number(top_spread$zscore[1], accuracy = 0.1) else "N/A",
    top_spread$spread_label[1],
    if (!is.na(top_spread$zscore[1]) && abs(top_spread$zscore[1]) > 2) "warning" else "neutral"
  )

  notes <- c(
    "Rolling correlations computed using a sliding window on front-month log returns.",
    "Treasury factor betas from OLS regression of commodity returns on UST level/slope/curvature changes.",
    "Cointegration residuals modelled with Ornstein-Uhlenbeck process via RTL::fitOU."
  )
  assumptions <- c(
    paste0("Rolling window: ", window, " trading days."),
    "Spreads computed as price of first leg minus second leg (front month).",
    "Connectedness score = mean absolute pairwise correlation across all selected markets."
  )

  list(
    correlation_matrix = correlation_matrix,
    correlation_timeseries = correlation_timeseries,
    spread_timeseries = spread_timeseries,
    spread_zscore = spread_zscore,
    beta_matrix = beta_matrix,
    pca_decomposition = pca_decomposition,
    treasury_betas = treasury_betas,
    connectedness_score = connectedness_score,
    correlation_breaks = correlation_breaks,
    coint_residual = coint_residual,
    rolling_beta_ts = rolling_beta_ts,
    kpis = kpis,
    notes = notes,
    assumptions = assumptions
  )
}
