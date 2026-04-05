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
      current = dplyr::last(.data$value),
      mean = mean(.data$value, na.rm = TRUE),
      std = sd(.data$value, na.rm = TRUE),
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

  list(
    correlation_matrix = correlation_matrix,
    correlation_timeseries = correlation_timeseries,
    spread_timeseries = spread_timeseries,
    spread_zscore = spread_zscore,
    beta_matrix = beta_matrix,
    pca_decomposition = pca_decomposition
  )
}
