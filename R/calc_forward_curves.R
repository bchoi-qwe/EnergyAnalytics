# ---- Term Structure Calculation Layer ----
# Produces compact forward curve analytics from the canonical snapshot.

ea_curve_regime <- function(m1_price, m1_m12, curvature) {
  if (is.na(m1_price) || !is.finite(m1_price) || abs(m1_price) < 1e-8 || is.na(m1_m12)) {
    return("Flat")
  }

  slope_pct <- m1_m12 / abs(m1_price)
  curvature_pct <- if (is.na(curvature)) 0 else curvature / abs(m1_price)

  if (abs(slope_pct) < 0.01 && abs(curvature_pct) < 0.01) {
    return("Flat")
  }

  if (slope_pct >= 0.20) {
    return("Steep Backwardation")
  }

  if (slope_pct >= 0.02) {
    if (curvature_pct <= -0.03) {
      return("Inverted Belly")
    }
    return("Backwardation")
  }

  if (slope_pct <= -0.20) {
    return("Super Contango")
  }

  if (slope_pct <= -0.02) {
    if (curvature_pct >= 0.03) {
      return("Humped Contango")
    }
    return("Contango")
  }

  "Flat"
}

ea_tenor_value <- function(values, tenors, target_tenor) {
  idx <- match(target_tenor, tenors)
  if (is.na(idx) || length(idx) == 0L) {
    return(NA_real_)
  }

  value <- values[[idx]]
  if (length(value) == 0L) {
    return(NA_real_)
  }

  as.numeric(value)
}

ea_build_curve_metrics <- function(curves) {
  if (nrow(curves) == 0L) {
    return(tibble::tibble(
      market = character(),
      date = as.Date(character()),
      m1_price = numeric(),
      m1_m2 = numeric(),
      m1_m6 = numeric(),
      m1_m12 = numeric(),
      curvature = numeric(),
      regime = character()
    ))
  }

  curves %>%
    dplyr::group_by(.data$market, .data$date) %>%
    dplyr::summarise(
      m1_price = {
        front_tenor <- suppressWarnings(min(.data$curve_point_num, na.rm = TRUE))
        ea_tenor_value(.data$value, .data$curve_point_num, front_tenor)
      },
      m1_m2 = {
        front_tenor <- suppressWarnings(min(.data$curve_point_num, na.rm = TRUE))
        front_val <- ea_tenor_value(.data$value, .data$curve_point_num, front_tenor)
        next_val <- ea_tenor_value(.data$value, .data$curve_point_num, front_tenor + 1L)
        if (is.na(front_val) || is.na(next_val)) NA_real_ else front_val - next_val
      },
      m1_m6 = {
        front_tenor <- suppressWarnings(min(.data$curve_point_num, na.rm = TRUE))
        front_val <- ea_tenor_value(.data$value, .data$curve_point_num, front_tenor)
        back_val <- ea_tenor_value(.data$value, .data$curve_point_num, front_tenor + 5L)
        if (is.na(front_val) || is.na(back_val)) NA_real_ else front_val - back_val
      },
      m1_m12 = {
        front_tenor <- suppressWarnings(min(.data$curve_point_num, na.rm = TRUE))
        front_val <- ea_tenor_value(.data$value, .data$curve_point_num, front_tenor)
        back_val <- ea_tenor_value(.data$value, .data$curve_point_num, front_tenor + 11L)
        if (is.na(front_val) || is.na(back_val)) NA_real_ else front_val - back_val
      },
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      curvature = dplyr::case_when(
        is.finite(.data$m1_m2) & is.finite(.data$m1_m6) ~ .data$m1_m2 - .data$m1_m6,
        TRUE ~ NA_real_
      ),
      regime = purrr::pmap_chr(
        list(.data$m1_price, .data$m1_m12, .data$curvature),
        ea_curve_regime
      )
    )
}

ea_build_calendar_spreads <- function(curves, spread_definitions) {
  empty_spreads <- tibble::tibble(
    date = as.Date(character()),
    market = character(),
    spread_label = character(),
    value = numeric()
  )

  if (nrow(curves) == 0L) {
    return(empty_spreads)
  }

  markets <- unique(curves$market)

  purrr::map_dfr(markets, function(mkt) {
    market_wide <- curves %>%
      dplyr::filter(.data$market == mkt) %>%
      ea_curve_to_wide() %>%
      dplyr::arrange(.data$date)

    tenor_cols <- setdiff(names(market_wide), c("date", "market"))
    tenor_nums <- suppressWarnings(readr::parse_number(tenor_cols))
    tenor_nums <- tenor_nums[is.finite(tenor_nums)]
    front_tenor <- if (length(tenor_nums) > 0L) min(tenor_nums, na.rm = TRUE) else NA_real_

    resolve_tenor_col <- function(target_tenor) {
      candidates <- c(sprintf("%02d", as.integer(target_tenor)), as.character(as.integer(target_tenor)))
      matches <- candidates[candidates %in% tenor_cols]
      if (length(matches) == 0L) NA_character_ else matches[[1]]
    }

    purrr::map_dfr(names(spread_definitions), function(lbl) {
      offsets <- spread_definitions[[lbl]]
      if (!is.finite(front_tenor) || length(offsets) != 2L) {
        return(empty_spreads)
      }

      legs <- purrr::map_chr(front_tenor + offsets, resolve_tenor_col)
      if (any(is.na(legs)) || !all(legs %in% names(market_wide))) {
        return(empty_spreads)
      }

      tibble::tibble(
        date = market_wide$date,
        market = mkt,
        spread_label = lbl,
        value = market_wide[[legs[[1]]]] - market_wide[[legs[[2]]]]
      ) %>%
        dplyr::filter(is.finite(.data$value))
    })
  })
}

ea_calc_forward_curves <- function(filters, history_context = "5y") {
  catalog <- ea_market_catalog()
  history_context <- ea_normalize_history_context(history_context)
  display_curves <- ea_load_filtered_curves(filters, apply_date_range = TRUE)
  all_curves <- ea_load_filtered_curves(filters, apply_date_range = FALSE)
  selected_markets <- ea_coalesce(filters$commodities, catalog$market)

  empty_curve <- tibble::tibble(
    market = character(),
    curve_point = character(),
    curve_point_num = numeric(),
    price = numeric(),
    label = character()
  )
  empty_history <- tibble::tibble(
    market = character(),
    snapshot_label = character(),
    snapshot_date = as.Date(character()),
    curve_point_num = numeric(),
    price = numeric()
  )
  empty_prompt <- tibble::tibble(
    date = as.Date(character()),
    market = character(),
    spread = numeric(),
    spread_mean = numeric(),
    spread_sd = numeric(),
    zscore = numeric(),
    p5 = numeric(),
    p25 = numeric(),
    p75 = numeric(),
    p95 = numeric()
  )
  empty_summary <- tibble::tibble(
    market = character(),
    snapshot_date = as.Date(character()),
    m1_price = numeric(),
    m1_m2 = numeric(),
    m1_m6 = numeric(),
    m1_m12 = numeric(),
    slope = numeric(),
    curvature = numeric(),
    regime = character(),
    regime_share = numeric(),
    m1_m2_percentile = numeric(),
    m1_m12_percentile = numeric(),
    prompt_zscore = numeric(),
    roll_yield_ann = numeric(),
    prompt_signal = character(),
    curve_signal = character()
  )
  empty_spreads <- tibble::tibble(
    date = as.Date(character()),
    market = character(),
    spread_label = character(),
    value = numeric()
  )

  if (nrow(display_curves) == 0L) {
    return(list(
      available_markets = character(),
      focus_market = NA_character_,
      history_context = history_context,
      history_context_label = ea_history_context_label(history_context),
      display_window = as.Date(c(NA, NA)),
      context_window = as.Date(c(NA, NA)),
      curve_snapshot = empty_curve,
      curve_history = empty_history,
      curve_change_heatmap = tibble::tibble(
        market = character(),
        date = as.Date(character()),
        curve_point_num = numeric(),
        value = numeric(),
        change = numeric()
      ),
      calendar_spreads = empty_spreads,
      calendar_spread_strip = tibble::tibble(
        market = character(),
        spread_label = character(),
        front_tenor = numeric(),
        current_spread = numeric(),
        zscore = numeric(),
        percentile = numeric(),
        rich_cheap = character()
      ),
      prompt_spread_ts = empty_prompt,
      roll_yield = tibble::tibble(
        market = character(),
        tenor_pair = character(),
        front_tenor = numeric(),
        front_price = numeric(),
        back_price = numeric(),
        spread = numeric(),
        roll_yield_ann = numeric()
      ),
      curve_pca = list(
        loadings = tibble::tibble(
          tenor = character(),
          PC1 = numeric(),
          PC2 = numeric(),
          PC3 = numeric()
        ),
        var_explained = tibble::tibble(
          component = character(),
          proportion = numeric()
        )
      ),
      treasury_overlay = tibble::tibble(
        date = as.Date(character()),
        market = character(),
        cmdty_spread = numeric(),
        slope_2s10s = numeric()
      ),
      structure_summary = empty_summary,
      kpis = tibble::tibble(
        title = character(),
        value = character(),
        delta = character(),
        status = character()
      ),
      notes = c("Forward-curve views use the selected display window."),
      assumptions = c("History context for percentiles and z-scores is empty because no display-window data is available.")
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

  focus_market <- available_markets[[1]]
  display_window <- as.Date(c(min(display_curves$date, na.rm = TRUE), max(display_curves$date, na.rm = TRUE)))
  context_start_date <- ea_history_context_start_date(
    end_date = display_window[[2]],
    history_context = history_context,
    min_date = min(all_curves$date, na.rm = TRUE)
  )
  context_curves <- all_curves %>%
    dplyr::filter(.data$date <= display_window[[2]], .data$date >= context_start_date)

  if (nrow(context_curves) == 0L) {
    context_curves <- display_curves
    context_start_date <- min(context_curves$date, na.rm = TRUE)
  }

  context_window <- as.Date(c(context_start_date, max(context_curves$date, na.rm = TRUE)))

  curve_snapshot <- display_curves %>%
    dplyr::group_by(.data$market) %>%
    dplyr::filter(.data$date == max(.data$date, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(
      market = .data$market,
      curve_point = .data$curve_point,
      curve_point_num = .data$curve_point_num,
      price = .data$value,
      label = ea_market_labels(.data$market, catalog)
    ) %>%
    dplyr::mutate(market_order = match(.data$market, available_markets)) %>%
    dplyr::arrange(.data$market_order, .data$curve_point_num) %>%
    dplyr::select(-"market_order")

  snapshot_offsets <- c("NOW" = 0L, "1M" = 30L, "3M" = 90L)
  curve_history <- purrr::map_dfr(available_markets, function(mkt) {
    market_curves <- display_curves %>%
      dplyr::filter(.data$market == mkt)

    if (nrow(market_curves) == 0L) {
      return(tibble::tibble())
    }

    market_dates <- sort(unique(market_curves$date), decreasing = TRUE)
    market_latest <- max(market_dates, na.rm = TRUE)

    purrr::map_dfr(names(snapshot_offsets), function(lbl) {
      target_date <- market_latest - snapshot_offsets[[lbl]]
      snap_date <- market_dates[[which.min(abs(as.numeric(market_dates - target_date)))]]

      market_curves %>%
        dplyr::filter(.data$date == snap_date) %>%
        dplyr::transmute(
          market = mkt,
          snapshot_label = lbl,
          snapshot_date = snap_date,
          curve_point_num = .data$curve_point_num,
          price = .data$value
        )
    })
  }) %>%
    dplyr::mutate(
      market_order = match(.data$market, available_markets),
      snapshot_order = match(.data$snapshot_label, names(snapshot_offsets))
    ) %>%
    dplyr::arrange(.data$market_order, .data$snapshot_order, .data$curve_point_num) %>%
    dplyr::select(-"market_order", -"snapshot_order")

  spread_definitions <- list(
    "M1-M2" = c(0L, 1L),
    "M1-M3" = c(0L, 2L),
    "M1-M6" = c(0L, 5L),
    "M1-M12" = c(0L, 11L)
  )

  calendar_spreads <- ea_build_calendar_spreads(display_curves, spread_definitions)
  context_spreads <- ea_build_calendar_spreads(context_curves, spread_definitions)

  spread_context_stats <- context_spreads %>%
    dplyr::group_by(.data$market, .data$spread_label) %>%
    dplyr::summarise(
      front_tenor = readr::parse_number(dplyr::first(.data$spread_label)),
      mean_spread = mean(.data$value, na.rm = TRUE),
      sd_spread = stats::sd(.data$value, na.rm = TRUE),
      p5 = stats::quantile(.data$value, probs = 0.05, na.rm = TRUE, names = FALSE),
      p25 = stats::quantile(.data$value, probs = 0.25, na.rm = TRUE, names = FALSE),
      p75 = stats::quantile(.data$value, probs = 0.75, na.rm = TRUE, names = FALSE),
      p95 = stats::quantile(.data$value, probs = 0.95, na.rm = TRUE, names = FALSE),
      history = list(.data$value[is.finite(.data$value)]),
      .groups = "drop"
    )

  current_spreads <- calendar_spreads %>%
    dplyr::group_by(.data$market, .data$spread_label) %>%
    dplyr::summarise(
      front_tenor = readr::parse_number(dplyr::first(.data$spread_label)),
      current_spread = dplyr::last(.data$value),
      .groups = "drop"
    )

  calendar_spread_strip <- current_spreads %>%
    dplyr::left_join(
      spread_context_stats,
      by = c("market", "spread_label", "front_tenor")
    ) %>%
    dplyr::mutate(
      zscore = dplyr::if_else(
        is.finite(.data$sd_spread) & .data$sd_spread > 0,
        (.data$current_spread - .data$mean_spread) / .data$sd_spread,
        NA_real_
      ),
      percentile = purrr::map2_dbl(
        .data$history,
        .data$current_spread,
        ~ {
          hist_vals <- .x[is.finite(.x)]
          if (length(hist_vals) == 0L || !is.finite(.y)) {
            return(NA_real_)
          }
          mean(hist_vals <= .y)
        }
      ),
      rich_cheap = dplyr::case_when(
        is.na(.data$percentile) ~ "Neutral",
        .data$percentile >= 0.90 ~ "Rich",
        .data$percentile <= 0.10 ~ "Cheap",
        TRUE ~ "Neutral"
      )
    ) %>%
    dplyr::select(
      "market", "spread_label", "front_tenor", "current_spread",
      "zscore", "percentile", "rich_cheap"
    ) %>%
    dplyr::arrange(.data$market, .data$front_tenor)

  prompt_context_stats <- spread_context_stats %>%
    dplyr::filter(.data$spread_label == "M1-M2") %>%
    dplyr::select(
      "market", "mean_spread", "sd_spread", "p5", "p25", "p75", "p95"
    )

  prompt_spread_ts <- calendar_spreads %>%
    dplyr::filter(.data$spread_label == "M1-M2") %>%
    dplyr::transmute(
      date = .data$date,
      market = .data$market,
      spread = .data$value
    ) %>%
    dplyr::left_join(prompt_context_stats, by = "market") %>%
    dplyr::mutate(
      zscore = dplyr::if_else(
        is.finite(.data$sd_spread) & .data$sd_spread > 0,
        (.data$spread - .data$mean_spread) / .data$sd_spread,
        NA_real_
      ),
      spread_mean = .data$mean_spread,
      spread_sd = .data$sd_spread
    ) %>%
    dplyr::select(
      "date", "market", "spread", "spread_mean", "spread_sd", "zscore",
      "p5", "p25", "p75", "p95"
    ) %>%
    dplyr::arrange(.data$market, .data$date)

  curve_change_heatmap <- display_curves %>%
    dplyr::group_by(.data$market, .data$curve_point_num) %>%
    dplyr::arrange(.data$date, .by_group = TRUE) %>%
    dplyr::mutate(change = .data$value - dplyr::lag(.data$value)) %>%
    dplyr::ungroup() %>%
    dplyr::select("market", "date", "curve_point_num", "value", "change")

  roll_yield <- curve_snapshot %>%
    dplyr::group_by(.data$market) %>%
    dplyr::arrange(.data$curve_point_num, .by_group = TRUE) %>%
    dplyr::mutate(
      back_tenor = dplyr::lead(.data$curve_point_num),
      back_price = dplyr::lead(.data$price),
      tenor_gap = .data$back_tenor - .data$curve_point_num,
      spread = .data$price - .data$back_price,
      tenor_pair = paste0("M", .data$curve_point_num, "-M", .data$back_tenor),
      roll_yield_ann = dplyr::if_else(
        !is.na(.data$back_price) & !is.na(.data$price) & !is.na(.data$tenor_gap) &
          .data$price != 0 & .data$tenor_gap > 0,
        (.data$spread / .data$price) * (12 / .data$tenor_gap),
        NA_real_
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(.data$back_price)) %>%
    dplyr::transmute(
      market = .data$market,
      tenor_pair = .data$tenor_pair,
      front_tenor = .data$curve_point_num,
      front_price = .data$price,
      back_price = .data$back_price,
      spread = .data$spread,
      roll_yield_ann = .data$roll_yield_ann
    )

  curve_pca <- tryCatch({
    focus_wide <- display_curves %>%
      dplyr::filter(.data$market == focus_market) %>%
      dplyr::select("date", "curve_point_num", "value") %>%
      tidyr::pivot_wider(names_from = "curve_point_num", values_from = "value", names_prefix = "M") %>%
      dplyr::arrange(.data$date) %>%
      dplyr::mutate(dplyr::across(-date, ~ .x - dplyr::lag(.x))) %>%
      dplyr::slice(-1L) %>%
      tidyr::drop_na()

    if (nrow(focus_wide) < 2L || ncol(focus_wide) <= 2L) {
      list(
        loadings = tibble::tibble(tenor = character(), PC1 = numeric(), PC2 = numeric(), PC3 = numeric()),
        var_explained = tibble::tibble(component = character(), proportion = numeric())
      )
    } else {
      pca_fit <- stats::prcomp(as.matrix(focus_wide[, -1, drop = FALSE]), center = TRUE, scale. = TRUE)
      explained <- (pca_fit$sdev ^ 2) / sum(pca_fit$sdev ^ 2)
      loadings <- as.data.frame(pca_fit$rotation[, 1:min(3, ncol(pca_fit$rotation)), drop = FALSE])
      colnames(loadings) <- paste0("PC", seq_len(ncol(loadings)))
      loadings$tenor <- rownames(loadings)

      for (pc in c("PC1", "PC2", "PC3")) {
        if (!pc %in% names(loadings)) {
          loadings[[pc]] <- NA_real_
        }
      }

      list(
        loadings = tibble::as_tibble(loadings) %>%
          dplyr::select("tenor", "PC1", "PC2", "PC3"),
        var_explained = tibble::tibble(
          component = paste0("PC", seq_along(explained)),
          proportion = as.numeric(explained)
        )
      )
    }
  }, error = function(e) {
    list(
      loadings = tibble::tibble(tenor = character(), PC1 = numeric(), PC2 = numeric(), PC3 = numeric()),
      var_explained = tibble::tibble(component = character(), proportion = numeric())
    )
  })

  treasury_overlay <- {
    ust_slope <- ea_load_dataset("ust_curve_long") %>%
      dplyr::filter(.data$curve_point_num %in% c(2, 10)) %>%
      dplyr::filter(.data$date >= display_window[[1]], .data$date <= display_window[[2]]) %>%
      dplyr::select("date", "curve_point_num", "value") %>%
      tidyr::pivot_wider(names_from = "curve_point_num", values_from = "value", names_prefix = "y") %>%
      dplyr::transmute(
        date = .data$date,
        slope_2s10s = .data$y10 - .data$y2
      )

    prompt_spread_ts %>%
      dplyr::transmute(
        date = .data$date,
        market = .data$market,
        cmdty_spread = .data$spread
      ) %>%
      dplyr::left_join(ust_slope, by = "date") %>%
      dplyr::filter(is.finite(.data$cmdty_spread) | is.finite(.data$slope_2s10s))
  }

  display_curve_metrics <- ea_build_curve_metrics(display_curves)
  context_curve_metrics <- ea_build_curve_metrics(context_curves)

  regime_distribution <- context_curve_metrics %>%
    dplyr::count(.data$market, .data$regime, name = "regime_days") %>%
    dplyr::group_by(.data$market) %>%
    dplyr::mutate(regime_share = .data$regime_days / sum(.data$regime_days)) %>%
    dplyr::ungroup() %>%
    dplyr::select("market", "regime", "regime_share")

  current_curve_metrics <- display_curve_metrics %>%
    dplyr::group_by(.data$market) %>%
    dplyr::filter(.data$date == max(.data$date, na.rm = TRUE)) %>%
    dplyr::slice_tail(n = 1L) %>%
    dplyr::ungroup() %>%
    dplyr::rename(snapshot_date = "date")

  prompt_context <- calendar_spread_strip %>%
    dplyr::filter(.data$spread_label == "M1-M2") %>%
    dplyr::transmute(
      market = .data$market,
      m1_m2_percentile = .data$percentile,
      prompt_zscore = .data$zscore,
      prompt_signal = .data$rich_cheap
    )

  curve_context <- calendar_spread_strip %>%
    dplyr::filter(.data$spread_label == "M1-M12") %>%
    dplyr::transmute(
      market = .data$market,
      m1_m12_percentile = .data$percentile,
      curve_signal = .data$rich_cheap
    )

  structure_summary <- current_curve_metrics %>%
    dplyr::left_join(
      roll_yield %>%
        dplyr::filter(.data$front_tenor == 1) %>%
        dplyr::transmute(market = .data$market, roll_yield_ann = .data$roll_yield_ann),
      by = "market"
    ) %>%
    dplyr::left_join(prompt_context, by = "market") %>%
    dplyr::left_join(curve_context, by = "market") %>%
    dplyr::left_join(regime_distribution, by = c("market", "regime")) %>%
    dplyr::mutate(
      slope = -.data$m1_m12
    ) %>%
    dplyr::select(
      "market", "snapshot_date", "m1_price", "m1_m2", "m1_m6", "m1_m12",
      "slope", "curvature", "regime", "regime_share", "m1_m2_percentile",
      "m1_m12_percentile", "prompt_zscore", "roll_yield_ann",
      "prompt_signal", "curve_signal"
    ) %>%
    dplyr::mutate(market_order = match(.data$market, available_markets)) %>%
    dplyr::arrange(.data$market_order) %>%
    dplyr::select(-"market_order")

  focus_summary <- structure_summary %>%
    dplyr::filter(.data$market == focus_market)

  kpis <- if (nrow(focus_summary) == 0L) {
    tibble::tibble(
      title = character(),
      value = character(),
      delta = character(),
      status = character()
    )
  } else {
    tibble::tribble(
      ~title, ~value, ~delta, ~status,
      "M1 Price",
      scales::number(focus_summary$m1_price[[1]], accuracy = 0.01),
      focus_market,
      "neutral",
      "Prompt Spread",
      if (is.finite(focus_summary$m1_m2[[1]])) scales::number(focus_summary$m1_m2[[1]], accuracy = 0.01) else "N/A",
      "M1-M2",
      "neutral",
      "Prompt Z",
      if (is.finite(focus_summary$prompt_zscore[[1]])) scales::number(focus_summary$prompt_zscore[[1]], accuracy = 0.01) else "N/A",
      ea_history_context_label(history_context),
      "neutral",
      "Regime",
      focus_summary$regime[[1]],
      "shape",
      dplyr::case_when(
        grepl("Backwardation", focus_summary$regime[[1]], fixed = TRUE) ~ "positive",
        grepl("Contango", focus_summary$regime[[1]], fixed = TRUE) ~ "warning",
        TRUE ~ "neutral"
      ),
      "Curve Percentile",
      if (is.finite(focus_summary$m1_m12_percentile[[1]])) scales::percent(focus_summary$m1_m12_percentile[[1]], accuracy = 1) else "N/A",
      paste0(ea_history_context_label(history_context), " M1-M12"),
      "neutral",
      "Roll Yield",
      if (is.finite(focus_summary$roll_yield_ann[[1]])) scales::percent(focus_summary$roll_yield_ann[[1]], accuracy = 0.1) else "N/A",
      "annualized",
      "neutral"
    )
  }

  list(
    available_markets = available_markets,
    focus_market = focus_market,
    history_context = history_context,
    history_context_label = ea_history_context_label(history_context),
    display_window = display_window,
    context_window = context_window,
    curve_snapshot = curve_snapshot,
    curve_history = curve_history,
    curve_change_heatmap = curve_change_heatmap,
    calendar_spreads = calendar_spreads,
    calendar_spread_strip = calendar_spread_strip,
    prompt_spread_ts = prompt_spread_ts,
    roll_yield = roll_yield,
    curve_pca = curve_pca,
    treasury_overlay = treasury_overlay,
    structure_summary = structure_summary,
    kpis = kpis,
    notes = c(
      "Snapshot and time-series views use the selected display window.",
      "Rich-cheap context is anchored to the local history basis selector."
    ),
    assumptions = c(
      paste0("Percentiles, z-scores, and regime frequency use the ", ea_history_context_label(history_context), " history window ending on the display-range end date."),
      "Regime classification is derived from M1-M12 slope and the relative shape of the prompt belly."
    )
  )
}
