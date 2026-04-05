# ---- Overview Calculation Layer ----
# Morning briefing: price snapshot, relative performance, curve shape, vol, drawdowns.

ea_calc_overview <- function(filters) {
  catalog <- ea_market_catalog()
  defaults <- ea_global_filter_defaults(catalog)
  markets <- ea_coalesce(filters$commodities, defaults$commodities)
  expiry_lo <- ea_coalesce(filters$expiry_range[1], 1L)
  expiry_hi <- ea_coalesce(filters$expiry_range[2], 12L)

  commodity_curves <- tryCatch(
    ea_load_dataset("commodity_curve_long"),
    error = function(e) NULL
  )

  if (is.null(commodity_curves)) {
    return(ea_overview_fallback(markets))
  }

  curves <- commodity_curves |>
    dplyr::filter(
      .data$market %in% markets,
      .data$curve_point_num >= expiry_lo,
      .data$curve_point_num <= expiry_hi
    )

  labels <- stats::setNames(catalog$label, catalog$market)
  latest_date <- max(curves$date, na.rm = TRUE)
  annualize <- sqrt(252)


  # Front contract series
  front <- curves |>
    dplyr::filter(.data$curve_point_num == min(.data$curve_point_num)) |>
    dplyr::arrange(.data$market, .data$date)

  # --- price_snapshot ---
  price_snapshot <- front |>
    dplyr::group_by(.data$market) |>
    dplyr::filter(dplyr::n() >= 2) |>
    dplyr::arrange(.data$date) |>
    dplyr::summarise(
      price = dplyr::last(.data$value),
      prev = dplyr::nth(.data$value, dplyr::n() - 1L),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      change = .data$price - .data$prev,
      change_pct = .data$change / .data$prev
    ) |>
    dplyr::select("market", "price", "change", "change_pct")

  # --- relative_performance: indexed to 100 over lookback ---
  lookback_start <- latest_date - 365L
  relative_performance <- front |>
    dplyr::filter(.data$date >= lookback_start) |>
    dplyr::group_by(.data$market) |>
    dplyr::arrange(.data$date) |>
    dplyr::mutate(indexed_value = .data$value / dplyr::first(.data$value) * 100) |>
    dplyr::ungroup() |>
    dplyr::select("date", "market", "indexed_value")

  # --- curve_structure_summary ---
  latest_curves <- curves |> dplyr::filter(.data$date == latest_date)

  curve_structure_summary <- latest_curves |>
    dplyr::group_by(.data$market) |>
    dplyr::summarise(
      m1 = .data$value[.data$curve_point_num == min(.data$curve_point_num)][1],
      m2 = .data$value[.data$curve_point_num == (min(.data$curve_point_num) + 1L)][1],
      back = .data$value[.data$curve_point_num == max(.data$curve_point_num)][1],
      .groups = "drop"
    ) |>
    dplyr::mutate(
      m1_m2_spread = .data$m1 - .data$m2,
      slope = .data$back - .data$m1,
      structure_label = dplyr::case_when(
        .data$slope > 0.01 ~ "Contango",
        .data$slope < -0.01 ~ "Backwardation",
        TRUE ~ "Flat"
      )
    ) |>
    dplyr::select("market", "m1_m2_spread", "slope", "structure_label")

  # --- correlation_snapshot ---
  # Filter out non-positive prices before log returns (e.g. NG can go negative)
  front_pos <- front |> dplyr::filter(.data$value > 0)

  returns_wide <- front_pos |>
    dplyr::group_by(.data$market) |>
    dplyr::arrange(.data$date) |>
    dplyr::mutate(
      prev_value = dplyr::lag(.data$value),
      log_return = dplyr::if_else(
        !is.na(.data$prev_value) & .data$prev_value > 0,
        log(.data$value / .data$prev_value),
        NA_real_
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(.data$log_return)) |>
    dplyr::select("date", "market", "log_return") |>
    tidyr::pivot_wider(names_from = "market", values_from = "log_return") |>
    dplyr::arrange(.data$date) |>
    stats::na.omit()

  market_cols <- setdiff(names(returns_wide), "date")
  tail_mat <- utils::tail(as.matrix(returns_wide[, market_cols, drop = FALSE]), 63)
  corr_mat <- stats::cor(tail_mat, use = "pairwise.complete.obs")

  correlation_snapshot <- expand.grid(
    market_x = market_cols, market_y = market_cols, stringsAsFactors = FALSE
  ) |>
    tibble::as_tibble() |>
    dplyr::rowwise() |>
    dplyr::mutate(correlation = corr_mat[.data$market_x, .data$market_y]) |>
    dplyr::ungroup()

  # --- spread_monitor ---
  spread_defs <- list(
    "CL-BRN" = c("CL", "BRN"),
    "RB-CL (Crack)" = c("RB", "CL"),
    "HO-CL (Crack)" = c("HO", "CL")
  )

  front_wide <- front |>
    dplyr::select("date", "market", "value") |>
    tidyr::pivot_wider(names_from = "market", values_from = "value") |>
    dplyr::arrange(.data$date)

  spread_monitor <- purrr::map_dfr(names(spread_defs), function(sp_lbl) {
    legs <- spread_defs[[sp_lbl]]
    if (!all(legs %in% names(front_wide))) return(tibble::tibble())

    spread_ts <- front_wide[[legs[1]]] - front_wide[[legs[2]]]
    valid <- !is.na(spread_ts)
    spread_ts <- spread_ts[valid]
    if (length(spread_ts) < 2) return(tibble::tibble())

    current <- spread_ts[length(spread_ts)]
    prev <- spread_ts[length(spread_ts) - 1L]
    rolling_mean <- mean(utils::tail(spread_ts, 63), na.rm = TRUE)
    rolling_sd <- stats::sd(utils::tail(spread_ts, 63), na.rm = TRUE)

    tibble::tibble(
      spread_label = sp_lbl,
      level = current,
      change = current - prev,
      zscore = if (rolling_sd > 0) (current - rolling_mean) / rolling_sd else 0
    )
  })

  # --- vol_snapshot ---
  vol_snapshot <- front_pos |>
    dplyr::group_by(.data$market) |>
    dplyr::arrange(.data$date) |>
    dplyr::mutate(
      prev_value = dplyr::lag(.data$value),
      log_return = dplyr::if_else(
        !is.na(.data$prev_value) & .data$prev_value > 0,
        log(.data$value / .data$prev_value),
        NA_real_
      )
    ) |>
    dplyr::filter(!is.na(.data$log_return)) |>
    dplyr::summarise(
      realized_vol_20d = stats::sd(utils::tail(.data$log_return, 20)) * annualize,
      all_vols = list(
        slider::slide_dbl(.data$log_return, stats::sd, .before = 19L, .complete = TRUE) * annualize
      ),
      .groups = "drop"
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      vol_percentile = {
        v <- unlist(.data$all_vols)
        v <- v[!is.na(v)]
        if (length(v) > 0) mean(v <= .data$realized_vol_20d) else NA_real_
      }
    ) |>
    dplyr::ungroup() |>
    dplyr::select("market", "realized_vol_20d", "vol_percentile")

  # --- drawdown_summary ---
  drawdown_summary <- front |>
    dplyr::group_by(.data$market) |>
    dplyr::arrange(.data$date) |>
    dplyr::summarise(
      peak = max(.data$value, na.rm = TRUE),
      peak_date = .data$date[which.max(.data$value)],
      current = dplyr::last(.data$value),
      latest = dplyr::last(.data$date),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      drawdown_pct = (.data$current - .data$peak) / .data$peak,
      days_from_peak = as.integer(.data$latest - .data$peak_date)
    ) |>
    dplyr::select("market", "drawdown_pct", "days_from_peak")

  # --- market_snapshot_table ---
  market_snapshot_table <- tryCatch({
    front_snap <- front |>
      dplyr::group_by(.data$market) |>
      dplyr::arrange(.data$date) |>
      dplyr::summarise(
        price         = dplyr::last(.data$value),
        chg_1d        = dplyr::last(.data$value) - dplyr::nth(.data$value, dplyr::n() - 1L),
        chg_1w        = dplyr::last(.data$value) - dplyr::nth(.data$value, max(1L, dplyr::n() - 5L)),
        chg_1m        = dplyr::last(.data$value) - dplyr::nth(.data$value, max(1L, dplyr::n() - 22L)),
        high_52w      = max(.data$value[.data$date >= (max(.data$date) - 365L)], na.rm = TRUE),
        low_52w       = min(.data$value[.data$date >= (max(.data$date) - 365L)], na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        pct_1d = dplyr::if_else(.data$price - .data$chg_1d > 0, .data$chg_1d / (.data$price - .data$chg_1d), NA_real_),
        pct_1w = dplyr::if_else(.data$price - .data$chg_1w > 0, .data$chg_1w / (.data$price - .data$chg_1w), NA_real_),
        pct_1m = dplyr::if_else(.data$price - .data$chg_1m > 0, .data$chg_1m / (.data$price - .data$chg_1m), NA_real_),
        percentile_52w = dplyr::if_else(
          .data$high_52w > .data$low_52w,
          (.data$price - .data$low_52w) / (.data$high_52w - .data$low_52w),
          NA_real_
        )
      ) |>
      dplyr::left_join(
        tibble::tibble(market = names(labels), label = unname(labels)),
        by = "market"
      ) |>
      dplyr::select(
        .data$market, .data$label, .data$price,
        .data$chg_1d, .data$pct_1d,
        .data$chg_1w, .data$pct_1w,
        .data$chg_1m, .data$pct_1m,
        .data$percentile_52w, .data$high_52w, .data$low_52w
      )
    front_snap
  }, error = function(e) {
    tibble::tibble(
      market = character(), label = character(), price = numeric(),
      chg_1d = numeric(), pct_1d = numeric(),
      chg_1w = numeric(), pct_1w = numeric(),
      chg_1m = numeric(), pct_1m = numeric(),
      percentile_52w = numeric(), high_52w = numeric(), low_52w = numeric()
    )
  })

  # --- top_anomalies ---
  top_anomalies <- tryCatch({
    spread_anomalies <- if (nrow(spread_monitor) > 0) {
      spread_monitor |>
        dplyr::select(.data$spread_label, .data$zscore) |>
        dplyr::mutate(anomaly_type = "Spread", label = .data$spread_label)
    } else {
      tibble::tibble(spread_label = character(), zscore = numeric(), anomaly_type = character(), label = character())
    }

    vol_anomalies <- if (nrow(vol_snapshot) > 0) {
      vol_snapshot |>
        dplyr::mutate(
          zscore = dplyr::if_else(!is.na(.data$vol_percentile), (.data$vol_percentile - 0.5) * 4, NA_real_),
          anomaly_type = "Vol",
          label = .data$market
        ) |>
        dplyr::select(.data$label, .data$zscore, .data$anomaly_type) |>
        dplyr::mutate(spread_label = .data$label)
    } else {
      tibble::tibble(spread_label = character(), zscore = numeric(), anomaly_type = character(), label = character())
    }

    dplyr::bind_rows(spread_anomalies, vol_anomalies) |>
      dplyr::filter(!is.na(.data$zscore)) |>
      dplyr::arrange(dplyr::desc(abs(.data$zscore))) |>
      utils::head(5L) |>
      dplyr::select(.data$label, .data$anomaly_type, .data$zscore)
  }, error = function(e) {
    tibble::tibble(label = character(), anomaly_type = character(), zscore = numeric())
  })

  # --- upcoming_events ---
  upcoming_events <- tryCatch({
    today <- Sys.Date()
    next_7 <- today + 7L

    # Hardcoded weekly releases
    releases <- tibble::tibble(
      event_date = {
        # Next Wednesday (WPSR)
        days_to_wed <- (3L - as.integer(format(today, "%u"))) %% 7L
        if (days_to_wed == 0L) days_to_wed <- 7L
        c(today + days_to_wed, today + days_to_wed + 7L)
      },
      event_name = c("EIA WPSR", "EIA WPSR"),
      event_type = c("release", "release")
    ) |>
      dplyr::filter(.data$event_date <= next_7)

    # Contract expirations from metadata if available
    expiry_events <- tryCatch({
      expiry_meta <- ea_load_dataset("contract_expiry_metadata")
      expiry_meta |>
        dplyr::filter(.data$last_trade >= today, .data$last_trade <= next_7,
                      .data$market %in% markets) |>
        dplyr::mutate(
          event_name = paste(.data$market, .data$contract_year, sprintf("%02d", .data$contract_month), "expiry"),
          event_type = "expiry"
        ) |>
        dplyr::select(event_date = .data$last_trade, .data$event_name, .data$event_type)
    }, error = function(e) {
      tibble::tibble(event_date = as.Date(character()), event_name = character(), event_type = character())
    })

    dplyr::bind_rows(releases, expiry_events) |>
      dplyr::arrange(.data$event_date)
  }, error = function(e) {
    tibble::tibble(event_date = as.Date(character()), event_name = character(), event_type = character())
  })

  # --- kpis ---
  # Best performing market by 1-month return
  best_mkt <- if (nrow(price_snapshot) > 0 && "change_pct" %in% names(price_snapshot)) {
    price_snapshot |> dplyr::slice_max(.data$change_pct, n = 1L, with_ties = FALSE)
  } else {
    NULL
  }

  # Top spread anomaly
  top_anomaly <- if (nrow(top_anomalies) > 0) top_anomalies[1, ] else NULL

  # Avg vol percentile
  avg_vol_pct <- if (nrow(vol_snapshot) > 0) {
    mean(vol_snapshot$vol_percentile, na.rm = TRUE)
  } else {
    NA_real_
  }

  # Most extreme drawdown
  worst_dd <- if (nrow(drawdown_summary) > 0) {
    drawdown_summary |> dplyr::slice_min(.data$drawdown_pct, n = 1L, with_ties = FALSE)
  } else {
    NULL
  }

  # Regime from curve structure
  backwardated <- curve_structure_summary |> dplyr::filter(.data$structure_label == "Backwardation")

  kpis <- tibble::tribble(
    ~title, ~value, ~delta, ~status,

    "Leader",
    if (!is.null(best_mkt) && nrow(best_mkt) > 0) best_mkt$market[1] else "N/A",
    if (!is.null(best_mkt) && nrow(best_mkt) > 0) {
      scales::percent(best_mkt$change_pct[1], accuracy = 0.01)
    } else { "" },
    if (!is.null(best_mkt) && nrow(best_mkt) > 0 && best_mkt$change_pct[1] > 0) "positive" else "neutral",

    "Top Anomaly",
    if (!is.null(top_anomaly) && nrow(top_anomaly) > 0) top_anomaly$label[1] else "N/A",
    if (!is.null(top_anomaly) && nrow(top_anomaly) > 0) {
      paste0("z=", round(top_anomaly$zscore[1], 1), " (", top_anomaly$anomaly_type[1], ")")
    } else { "" },
    if (!is.null(top_anomaly) && nrow(top_anomaly) > 0 && abs(top_anomaly$zscore[1]) > 2) "warning" else "neutral",

    "Vol Regime",
    if (!is.na(avg_vol_pct)) scales::percent(avg_vol_pct, accuracy = 1) else "N/A",
    "avg vol percentile",
    if (!is.na(avg_vol_pct) && avg_vol_pct > 0.75) "warning" else "neutral",

    "Max Drawdown",
    if (!is.null(worst_dd) && nrow(worst_dd) > 0) {
      scales::percent(worst_dd$drawdown_pct[1], accuracy = 0.1)
    } else { "N/A" },
    if (!is.null(worst_dd) && nrow(worst_dd) > 0) worst_dd$market[1] else "",
    if (!is.null(worst_dd) && nrow(worst_dd) > 0 && worst_dd$drawdown_pct[1] < -0.20) "warning" else "neutral",

    "Backwardated",
    if (nrow(backwardated) > 0) paste(backwardated$market, collapse = ", ") else "None",
    "curve structure",
    if (nrow(backwardated) > 0) "positive" else "neutral",

    "Upcoming Events",
    if (nrow(upcoming_events) > 0) as.character(nrow(upcoming_events)) else "0",
    "next 7 days",
    "neutral"
  )

  # --- notes and assumptions ---
  notes <- c(
    "Price snapshot uses front contract (M1) daily prices from the canonical snapshot.",
    "Relative performance indexed to 100 at 1-year lookback start.",
    "Vol percentile rank computed over the full price history."
  )
  assumptions <- c(
    "Spread z-scores use a 63-day rolling window.",
    "Top anomalies are the 5 most extreme z-scores across spreads and vol.",
    "Upcoming events includes EIA WPSR (Wednesday) and contract expirations within 7 days."
  )

  list(
    price_snapshot = price_snapshot,
    relative_performance = relative_performance,
    curve_structure_summary = curve_structure_summary,
    correlation_snapshot = correlation_snapshot,
    spread_monitor = spread_monitor,
    vol_snapshot = vol_snapshot,
    drawdown_summary = drawdown_summary,
    market_snapshot_table = market_snapshot_table,
    top_anomalies = top_anomalies,
    upcoming_events = upcoming_events,
    kpis = kpis,
    notes = notes,
    assumptions = assumptions
  )
}

# Fallback when no snapshot data is available
ea_overview_fallback <- function(markets) {
  empty_df <- tibble::tibble(market = character())
  list(
    price_snapshot = dplyr::mutate(empty_df, price = numeric(), change = numeric(), change_pct = numeric()),
    relative_performance = tibble::tibble(date = as.Date(character()), market = character(), indexed_value = numeric()),
    curve_structure_summary = dplyr::mutate(empty_df, m1_m2_spread = numeric(), slope = numeric(), structure_label = character()),
    correlation_snapshot = tibble::tibble(market_x = character(), market_y = character(), correlation = numeric()),
    spread_monitor = tibble::tibble(spread_label = character(), level = numeric(), change = numeric(), zscore = numeric()),
    vol_snapshot = dplyr::mutate(empty_df, realized_vol_20d = numeric(), vol_percentile = numeric()),
    drawdown_summary = dplyr::mutate(empty_df, drawdown_pct = numeric(), days_from_peak = integer()),
    market_snapshot_table = tibble::tibble(
      market = character(), label = character(), price = numeric(),
      chg_1d = numeric(), pct_1d = numeric(), chg_1w = numeric(), pct_1w = numeric(),
      chg_1m = numeric(), pct_1m = numeric(), percentile_52w = numeric(),
      high_52w = numeric(), low_52w = numeric()
    ),
    top_anomalies = tibble::tibble(label = character(), anomaly_type = character(), zscore = numeric()),
    upcoming_events = tibble::tibble(event_date = as.Date(character()), event_name = character(), event_type = character()),
    kpis = tibble::tibble(title = character(), value = character(), delta = character(), status = character()),
    notes = character(),
    assumptions = character()
  )
}
