# ---- Seasonality Calculation Layer ----
# Seasonal patterns, historical overlays, and mean reversion.

ea_calc_seasonality <- function(filters, history_context = "5y") {
  catalog <- ea_market_catalog()
  history_context <- ea_normalize_history_context(history_context)
  selected_markets <- ea_coalesce(filters$commodities, catalog$market)

  empty_overlay <- tibble::tibble(day_of_year = integer(), year = integer(), market = character(), indexed_value = numeric())
  empty_range <- tibble::tibble(day_of_year = integer(), market = character(), avg = numeric(), p25 = numeric(), p75 = numeric(), min = numeric(), max = numeric())
  empty_returns <- tibble::tibble(market = character(), period = character(), avg_return = numeric(), hit_rate = numeric())
  empty_mr <- tibble::tibble(market = character(), theta = numeric(), mu = numeric(), sigma = numeric(), half_life = numeric())
  empty_dev <- tibble::tibble(date = as.Date(character()), market = character(), percentile = numeric(), zscore = numeric())
  empty_stl <- tibble::tibble(date = as.Date(character()), market = character(), trend = numeric(), seasonal = numeric(), remainder = numeric())
  empty_spreads <- tibble::tibble(day_of_year = integer(), market = character(), avg = numeric(), p10 = numeric(), p25 = numeric(), p75 = numeric(), p90 = numeric(), current_spread = numeric(), current_percentile = numeric())
  empty_vol <- tibble::tibble(day_of_year = integer(), market = character(), avg_vol = numeric(), p25_vol = numeric(), p75_vol = numeric(), current_vol = numeric())
  empty_hedge <- tibble::tibble(month = character(), r_squared = numeric(), market = character())
  empty_summary <- tibble::tibble(market = character(), anomaly_score = numeric(), current_percentile = numeric(), direction = character(), days_to_inflection = numeric())

  display_curves <- ea_load_filtered_curves(filters, apply_date_range = TRUE)
  all_curves <- ea_load_filtered_curves(filters, apply_date_range = FALSE)

  if (nrow(all_curves) == 0L || length(selected_markets) == 0L) {
    return(list(
      available_markets = character(),
      history_context = history_context,
      history_context_label = ea_history_context_label(history_context),
      display_window = as.Date(c(NA, NA)),
      context_window = as.Date(c(NA, NA)),
      anchor_date = as.Date(NA),
      seasonal_overlay = empty_overlay,
      seasonal_range = empty_range,
      seasonal_returns = empty_returns,
      mean_reversion = empty_mr,
      seasonal_deviation = empty_dev,
      stl_decomposition = empty_stl,
      seasonal_spreads = empty_spreads,
      seasonal_vol = empty_vol,
      seasonal_hedge_effectiveness = empty_hedge,
      seasonal_summary = empty_summary,
      kpis = tibble::tibble(title = character(), value = character(), delta = character(), status = character()),
      notes = c("Seasonality needs front-contract history."),
      assumptions = c("No display-window data is available.")
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
      anchor_date = as.Date(NA),
      seasonal_overlay = empty_overlay,
      seasonal_range = empty_range,
      seasonal_returns = empty_returns,
      mean_reversion = empty_mr,
      seasonal_deviation = empty_dev,
      stl_decomposition = empty_stl,
      seasonal_spreads = empty_spreads,
      seasonal_vol = empty_vol,
      seasonal_hedge_effectiveness = empty_hedge,
      seasonal_summary = empty_summary,
      kpis = tibble::tibble(title = character(), value = character(), delta = character(), status = character()),
      notes = c("Seasonality uses the selected display range to choose the anchor date. The current display range contains no data."),
      assumptions = c("Pick a display range that contains at least one observation.")
    ))
  }

  window_info <- ea_resolve_analysis_windows(display_curves$date, all_curves$date, history_context)
  display_window <- window_info$display_window
  context_window <- window_info$context_window
  anchor_date <- window_info$anchor_date
  anchor_year <- as.integer(format(anchor_date, "%Y"))
  anchor_day <- as.integer(format(anchor_date, "%j"))
  context_curves <- all_curves %>%
    dplyr::filter(.data$date >= context_window[[1]], .data$date <= context_window[[2]])

  front_all <- all_curves %>%
    dplyr::filter(.data$curve_point_num == 1, .data$date <= anchor_date) %>%
    dplyr::select(.data$date, .data$market, .data$value)
  front_context <- front_all %>%
    dplyr::filter(.data$date >= context_window[[1]], .data$date <= context_window[[2]])

  indexed_front_full <- front_all %>%
    dplyr::mutate(
      year = as.integer(format(.data$date, "%Y")),
      day_of_year = as.integer(format(.data$date, "%j"))
    ) %>%
    dplyr::group_by(.data$market, .data$year) %>%
    dplyr::arrange(.data$date, .by_group = TRUE) %>%
    dplyr::mutate(
      indexed_value = .data$value / dplyr::first(.data$value) * 100,
      ytd_return = .data$indexed_value / 100 - 1
    ) %>%
    dplyr::ungroup()

  indexed_front_ytd <- indexed_front_full %>%
    dplyr::filter(.data$day_of_year <= anchor_day)

  context_years <- indexed_front_ytd %>%
    dplyr::filter(.data$date >= context_window[[1]], .data$year < anchor_year) %>%
    dplyr::distinct(.data$year) %>%
    dplyr::pull(.data$year)

  seasonal_overlay <- indexed_front_ytd %>%
    dplyr::filter((.data$year %in% context_years) | .data$year == anchor_year) %>%
    dplyr::select(.data$day_of_year, .data$year, .data$market, .data$indexed_value, .data$ytd_return)

  historical_indexed <- indexed_front_full %>%
    dplyr::filter(.data$year %in% context_years) %>%
    dplyr::select(.data$day_of_year, .data$year, .data$market, .data$indexed_value, .data$ytd_return)

  seasonal_range <- historical_indexed %>%
    dplyr::group_by(.data$market, .data$day_of_year) %>%
    dplyr::summarise(
      avg = mean(.data$indexed_value, na.rm = TRUE),
      p25 = stats::quantile(.data$indexed_value, 0.25, na.rm = TRUE, names = FALSE),
      p75 = stats::quantile(.data$indexed_value, 0.75, na.rm = TRUE, names = FALSE),
      min = min(.data$indexed_value, na.rm = TRUE),
      max = max(.data$indexed_value, na.rm = TRUE),
      .groups = "drop"
    )

  current_indexed <- seasonal_overlay %>%
    dplyr::filter(.data$year == anchor_year) %>%
    dplyr::select(.data$day_of_year, .data$market, current_val = .data$indexed_value)

  seasonal_deviation <- current_indexed %>%
    dplyr::left_join(
      historical_indexed %>%
        dplyr::group_by(.data$market, .data$day_of_year) %>%
        dplyr::summarise(
          values = list(.data$indexed_value),
          mean_val = mean(.data$indexed_value, na.rm = TRUE),
          sd_val = stats::sd(.data$indexed_value, na.rm = TRUE),
          .groups = "drop"
        ),
      by = c("market", "day_of_year")
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      percentile = if (length(.data$values) > 0) mean(.data$values <= .data$current_val, na.rm = TRUE) else NA_real_,
      zscore = if (is.finite(.data$sd_val) && .data$sd_val > 0) (.data$current_val - .data$mean_val) / .data$sd_val else NA_real_
    ) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(
      date = as.Date(paste0(anchor_year, "-01-01")) + .data$day_of_year - 1L,
      market = .data$market,
      percentile = .data$percentile,
      zscore = .data$zscore
    )

  seasonal_returns <- front_context %>%
    dplyr::mutate(
      year = as.integer(format(.data$date, "%Y")),
      period = format(.data$date, "%b")
    ) %>%
    dplyr::filter(.data$year < anchor_year) %>%
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
    dplyr::filter(!is.na(.data$log_return)) %>%
    dplyr::group_by(.data$market, .data$year, .data$period) %>%
    dplyr::summarise(
      period_return = exp(sum(.data$log_return, na.rm = TRUE)) - 1,
      .groups = "drop"
    ) %>%
    dplyr::group_by(.data$market, .data$period) %>%
    dplyr::summarise(
      avg_return = mean(.data$period_return, na.rm = TRUE),
      hit_rate = mean(.data$period_return > 0, na.rm = TRUE),
      .groups = "drop"
    )

  mean_reversion <- purrr::map_dfr(available_markets, function(mkt) {
    mkt_curves <- context_curves %>%
      dplyr::filter(.data$market == mkt, .data$curve_point_num %in% c(1, 2))

    wide <- mkt_curves %>%
      dplyr::select(.data$date, .data$curve_point, .data$value) %>%
      tidyr::pivot_wider(names_from = .data$curve_point, values_from = .data$value) %>%
      dplyr::arrange(.data$date) %>%
      stats::na.omit()

    if (nrow(wide) < 30 || !"01" %in% names(wide) || !"02" %in% names(wide)) {
      return(tibble::tibble(market = mkt, theta = NA_real_, mu = NA_real_, sigma = NA_real_, half_life = NA_real_))
    }

    spread <- wide[["01"]] - wide[["02"]]
    tryCatch({
      ou <- RTL::fitOU(spread = spread)
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

  stl_decomposition <- tryCatch({
    primary_mkt <- available_markets[[1]]
    front_primary <- front_all %>%
      dplyr::filter(.data$market == primary_mkt) %>%
      dplyr::arrange(.data$date)

    if (nrow(front_primary) < 252 * 2L) stop("need 2y data for STL")

    stl_fit <- stats::stl(stats::ts(front_primary$value, frequency = 252), s.window = "periodic")
    tibble::tibble(
      date = front_primary$date,
      market = primary_mkt,
      trend = as.numeric(stl_fit$time.series[, "trend"]),
      seasonal = as.numeric(stl_fit$time.series[, "seasonal"]),
      remainder = as.numeric(stl_fit$time.series[, "remainder"])
    ) %>%
      dplyr::filter(.data$date >= context_window[[1]], .data$date <= anchor_date)
  }, error = function(...) empty_stl)

  seasonal_spreads <- purrr::map_dfr(available_markets, function(mkt) {
    spread_history <- all_curves %>%
      dplyr::filter(.data$market == mkt, .data$curve_point_num %in% c(1L, 2L), .data$date <= anchor_date) %>%
      dplyr::select(.data$date, .data$curve_point_num, .data$value) %>%
      tidyr::pivot_wider(names_from = .data$curve_point_num, values_from = .data$value) %>%
      dplyr::arrange(.data$date) %>%
      dplyr::filter(!is.na(.data$`1`), !is.na(.data$`2`)) %>%
      dplyr::mutate(
        spread = .data$`1` - .data$`2`,
        year = as.integer(format(.data$date, "%Y")),
        day_of_year = as.integer(format(.data$date, "%j"))
      ) %>%
      dplyr::filter(.data$day_of_year <= anchor_day)

    hist_years <- spread_history %>%
      dplyr::filter(.data$date >= context_window[[1]], .data$year < anchor_year)
    current_year <- spread_history %>%
      dplyr::filter(.data$year == anchor_year)

    if (nrow(hist_years) < 30L) {
      return(tibble::tibble())
    }

    current_percentiles <- current_year %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        current_percentile = {
          hist_slice <- hist_years$spread[hist_years$day_of_year == .data$day_of_year]
          if (length(hist_slice) == 0L) NA_real_ else mean(hist_slice <= .data$spread, na.rm = TRUE)
        }
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(.data$day_of_year, current_spread = .data$spread, .data$current_percentile)

    hist_years %>%
      dplyr::group_by(.data$day_of_year) %>%
      dplyr::summarise(
        avg = mean(.data$spread, na.rm = TRUE),
        p10 = stats::quantile(.data$spread, 0.10, na.rm = TRUE, names = FALSE),
        p25 = stats::quantile(.data$spread, 0.25, na.rm = TRUE, names = FALSE),
        p75 = stats::quantile(.data$spread, 0.75, na.rm = TRUE, names = FALSE),
        p90 = stats::quantile(.data$spread, 0.90, na.rm = TRUE, names = FALSE),
        .groups = "drop"
      ) %>%
      dplyr::left_join(current_percentiles, by = "day_of_year") %>%
      dplyr::mutate(market = mkt)
  })

  seasonal_vol <- purrr::map_dfr(available_markets, function(mkt) {
    mkt_front <- front_all %>%
      dplyr::filter(.data$market == mkt) %>%
      dplyr::arrange(.data$date) %>%
      dplyr::mutate(
        prev_value = dplyr::lag(.data$value),
        raw_log_return = suppressWarnings(log(.data$value / .data$prev_value)),
        log_return = dplyr::if_else(
          is.finite(.data$value) & is.finite(.data$prev_value) & .data$value > 0 & .data$prev_value > 0,
          .data$raw_log_return,
          NA_real_
        ),
        day_of_year = as.integer(format(.data$date, "%j")),
        year = as.integer(format(.data$date, "%Y"))
      ) %>%
      dplyr::filter(!is.na(.data$log_return), .data$day_of_year <= anchor_day)

    if (nrow(mkt_front) < 20L) {
      return(tibble::tibble())
    }

    mkt_front$rv20 <- slider::slide_dbl(
      mkt_front$log_return,
      ~ stats::sd(.x, na.rm = TRUE) * sqrt(252),
      .before = 19L,
      .complete = TRUE
    )

    hist_years <- mkt_front %>%
      dplyr::filter(.data$date >= context_window[[1]], .data$year < anchor_year, !is.na(.data$rv20))
    current_year <- mkt_front %>%
      dplyr::filter(.data$year == anchor_year, !is.na(.data$rv20)) %>%
      dplyr::select(.data$day_of_year, current_vol = .data$rv20)

    if (nrow(hist_years) < 20L) {
      return(tibble::tibble())
    }

    hist_years %>%
      dplyr::group_by(.data$day_of_year) %>%
      dplyr::summarise(
        avg_vol = mean(.data$rv20, na.rm = TRUE),
        p25_vol = stats::quantile(.data$rv20, 0.25, na.rm = TRUE, names = FALSE),
        p75_vol = stats::quantile(.data$rv20, 0.75, na.rm = TRUE, names = FALSE),
        .groups = "drop"
      ) %>%
      dplyr::left_join(current_year, by = "day_of_year") %>%
      dplyr::mutate(market = mkt)
  })

  seasonal_hedge_effectiveness <- tryCatch({
    if (length(available_markets) < 2L) {
      empty_hedge
    } else {
      wide_returns <- front_context %>%
        dplyr::mutate(year = as.integer(format(.data$date, "%Y"))) %>%
        dplyr::filter(.data$year < anchor_year) %>%
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
        dplyr::filter(!is.na(.data$log_return)) %>%
        dplyr::select(.data$date, .data$market, .data$log_return) %>%
        tidyr::pivot_wider(names_from = "market", values_from = "log_return") %>%
        dplyr::arrange(.data$date) %>%
        stats::na.omit()

      if (ncol(wide_returns) < 3L) {
        empty_hedge
      } else {
        x_col <- names(wide_returns)[[2]]
        y_cols <- names(wide_returns)[seq(3, ncol(wide_returns))]

        purrr::map_dfr(y_cols, function(y_col) {
          month_data <- wide_returns %>% dplyr::mutate(month = format(.data$date, "%b"))
          purrr::map_dfr(month.abb, function(mon) {
            df_m <- month_data %>% dplyr::filter(.data$month == mon)
            if (nrow(df_m) < 10L) {
              return(tibble::tibble(month = mon, r_squared = NA_real_, market = y_col))
            }
            tibble::tibble(
              month = mon,
              r_squared = broom::glance(stats::lm(df_m[[y_col]] ~ df_m[[x_col]]))$r.squared,
              market = y_col
            )
          })
        })
      }
    }
  }, error = function(...) empty_hedge)

  next_turning_point_days <- function(mkt) {
    avg_path <- seasonal_range %>%
      dplyr::filter(.data$market == mkt) %>%
      dplyr::arrange(.data$day_of_year)

    if (nrow(avg_path) < 3L) {
      return(NA_real_)
    }

    turning_days <- avg_path %>%
      dplyr::mutate(
        prev_avg = dplyr::lag(.data$avg),
        next_avg = dplyr::lead(.data$avg)
      ) %>%
      dplyr::filter(
        !is.na(.data$prev_avg),
        !is.na(.data$next_avg),
        (.data$avg >= .data$prev_avg & .data$avg > .data$next_avg) |
          (.data$avg <= .data$prev_avg & .data$avg < .data$next_avg)
      ) %>%
      dplyr::pull(.data$day_of_year)

    if (length(turning_days) == 0L) {
      return(NA_real_)
    }

    day_offsets <- (turning_days - anchor_day) %% 366L
    day_offsets <- day_offsets[day_offsets > 0L]

    if (length(day_offsets) == 0L) {
      return(NA_real_)
    }

    min(day_offsets, na.rm = TRUE)
  }

  seasonal_summary <- purrr::map_dfr(available_markets, function(mkt) {
    dev <- seasonal_deviation %>% dplyr::filter(.data$market == mkt)
    ret <- seasonal_returns %>% dplyr::filter(.data$market == mkt)

    current_percentile <- if (nrow(dev) > 0L) utils::tail(dev$percentile, 1L) else NA_real_
    anomaly_score <- if (is.finite(current_percentile)) abs(current_percentile - 0.5) * 2 else NA_real_

    current_month <- format(anchor_date, "%b")
    next_month_idx <- (which(month.abb == current_month) %% 12L) + 1L
    next_month <- month.abb[[next_month_idx]]
    next_direction <- if (nrow(ret) > 0L && next_month %in% ret$period) {
      avg_r <- ret$avg_return[ret$period == next_month]
      if (length(avg_r) > 0L && is.finite(avg_r[[1]]) && avg_r[[1]] > 0) "up" else "down"
    } else {
      NA_character_
    }

    tibble::tibble(
      market = mkt,
      anomaly_score = anomaly_score,
      current_percentile = current_percentile,
      direction = next_direction,
      days_to_inflection = next_turning_point_days(mkt)
    )
  })

  primary_mkt <- available_markets[[1]]
  primary_dev <- seasonal_deviation %>% dplyr::filter(.data$market == primary_mkt)
  primary_ret <- seasonal_returns %>% dplyr::filter(.data$market == primary_mkt)
  primary_mr <- mean_reversion %>% dplyr::filter(.data$market == primary_mkt)
  primary_sum <- seasonal_summary %>% dplyr::filter(.data$market == primary_mkt)

  best_month <- if (nrow(primary_ret) > 0L) {
    primary_ret[which.max(primary_ret$avg_return), , drop = FALSE]
  } else {
    NULL
  }

  kpis <- tibble::tribble(
    ~title, ~value, ~delta, ~status,
    "Current Percentile",
    if (nrow(primary_dev) > 0L && is.finite(utils::tail(primary_dev$percentile, 1L))) scales::percent(utils::tail(primary_dev$percentile, 1L), accuracy = 1) else "N/A",
    ea_history_context_label(history_context),
    "neutral",

    "Best Month",
    if (!is.null(best_month)) best_month$period[[1]] else "N/A",
    if (!is.null(best_month) && is.finite(best_month$avg_return[[1]])) scales::percent(best_month$avg_return[[1]], accuracy = 0.1) else "",
    "neutral",

    "Anomaly Score",
    if (nrow(primary_sum) > 0L && is.finite(primary_sum$anomaly_score[[1]])) scales::number(primary_sum$anomaly_score[[1]], accuracy = 0.01) else "N/A",
    "0 = normal",
    "neutral",

    "OU Half-Life",
    if (nrow(primary_mr) > 0L && is.finite(primary_mr$half_life[[1]])) paste0(round(primary_mr$half_life[[1]]), "d") else "N/A",
    "M1-M2",
    "neutral",

    "Seasonal Direction",
    if (nrow(primary_sum) > 0L && !is.na(primary_sum$direction[[1]])) toupper(primary_sum$direction[[1]]) else "N/A",
    "next month",
    "neutral"
  )

  list(
    available_markets = available_markets,
    history_context = history_context,
    history_context_label = ea_history_context_label(history_context),
    display_window = display_window,
    context_window = context_window,
    anchor_date = anchor_date,
    seasonal_overlay = seasonal_overlay,
    seasonal_range = seasonal_range,
    seasonal_returns = seasonal_returns,
    mean_reversion = mean_reversion,
    seasonal_deviation = seasonal_deviation,
    stl_decomposition = stl_decomposition,
    seasonal_spreads = seasonal_spreads,
    seasonal_vol = seasonal_vol,
    seasonal_hedge_effectiveness = seasonal_hedge_effectiveness,
    seasonal_summary = seasonal_summary,
    kpis = kpis,
    notes = c(
      "Seasonality uses the end of the selected display range as the anchor date for the current year path.",
      paste0("Historical seasonal bands and hit rates use the ", ea_history_context_label(history_context), " history window prior to that anchor."),
      "Prompt-spread seasonality and mean-reversion are computed from the term-structure history available up to the anchor date."
    ),
    assumptions = c(
      "Seasonal overlays are indexed to 100 at the first trading day of each year.",
      "Year-on-year comparisons are clipped to the same day-of-year as the selected anchor date.",
      "Realized volatility seasonality uses a 20-day rolling window annualized at sqrt(252)."
    )
  )
}
