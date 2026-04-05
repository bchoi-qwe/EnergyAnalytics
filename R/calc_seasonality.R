# ---- Seasonality Calculation Layer ----
# Seasonal patterns, historical overlays, and mean reversion.

ea_calc_seasonality <- function(filters) {
  catalog <- ea_market_catalog()
  curves <- ea_load_filtered_curves(filters)
  markets <- unique(curves$market)

  # Front contract prices
  front <- curves |>
    dplyr::filter(.data$curve_point_num == 1) |>
    dplyr::select(.data$date, .data$market, .data$value)

  latest_year <- as.integer(format(max(front$date, na.rm = TRUE), "%Y"))

  # --- seasonal_overlay: current year vs historical years, indexed to 100 ---
  seasonal_overlay <- front |>
    dplyr::mutate(
      year = as.integer(format(.data$date, "%Y")),
      day_of_year = as.integer(format(.data$date, "%j"))
    ) |>
    dplyr::group_by(.data$market, .data$year) |>
    dplyr::mutate(indexed_value = .data$value / dplyr::first(.data$value) * 100) |>
    dplyr::ungroup() |>
    dplyr::filter(.data$year >= latest_year - 5L) |>
    dplyr::select(.data$day_of_year, .data$year, .data$market, .data$indexed_value)

  # --- seasonal_range: 5-year min/max/mean band ---
  historical_indexed <- seasonal_overlay |>
    dplyr::filter(.data$year < latest_year, .data$year >= latest_year - 5L)

  seasonal_range <- historical_indexed |>
    dplyr::group_by(.data$market, .data$day_of_year) |>
    dplyr::summarise(
      avg = mean(.data$indexed_value, na.rm = TRUE),
      min = min(.data$indexed_value, na.rm = TRUE),
      max = max(.data$indexed_value, na.rm = TRUE),
      .groups = "drop"
    )

  # --- seasonal_returns: avg monthly return and hit rate ---
  monthly_returns <- front |>
    dplyr::arrange(.data$market, .data$date) |>
    dplyr::group_by(.data$market) |>
    dplyr::mutate(
      log_return = log(.data$value / dplyr::lag(.data$value)),
      period = format(.data$date, "%b")
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(.data$log_return))

  seasonal_returns <- monthly_returns |>
    dplyr::group_by(.data$market, .data$period) |>
    dplyr::summarise(
      avg_return = mean(.data$log_return, na.rm = TRUE),
      hit_rate = mean(.data$log_return > 0, na.rm = TRUE),
      .groups = "drop"
    )

  # --- mean_reversion: OU fit on M1-M2 spread ---
  mean_reversion <- purrr::map_dfr(markets, function(mkt) {
    mkt_curves <- curves |>
      dplyr::filter(.data$market == mkt, .data$curve_point_num %in% c(1, 2))

    wide <- mkt_curves |>
      dplyr::select(.data$date, .data$curve_point, .data$value) |>
      tidyr::pivot_wider(names_from = .data$curve_point, values_from = .data$value) |>
      dplyr::arrange(.data$date) |>
      stats::na.omit()

    if (nrow(wide) < 30 || !"01" %in% names(wide) || !"02" %in% names(wide)) {
      return(tibble::tibble(
        market = mkt, theta = NA_real_, mu = NA_real_,
        sigma = NA_real_, half_life = NA_real_
      ))
    }

    spread <- wide[["01"]] - wide[["02"]]

    tryCatch({
      ou <- RTL::fitOU(spread = spread)
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

  # --- seasonal_deviation: current year percentile in historical distribution ---
  current_indexed <- seasonal_overlay |>
    dplyr::filter(.data$year == latest_year) |>
    dplyr::select(.data$day_of_year, .data$market, current_val = .data$indexed_value)

  seasonal_deviation <- current_indexed |>
    dplyr::left_join(
      historical_indexed |>
        dplyr::group_by(.data$market, .data$day_of_year) |>
        dplyr::summarise(values = list(.data$indexed_value), .groups = "drop"),
      by = c("market", "day_of_year")
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      percentile = if (length(.data$values) > 0) {
        mean(.data$values <= .data$current_val, na.rm = TRUE)
      } else {
        NA_real_
      }
    ) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      date = as.Date(paste0(latest_year, "-01-01")) + .data$day_of_year - 1L,
      market = .data$market,
      percentile = .data$percentile
    )

  # --- stl_decomposition: STL decomposition of primary market front prices ---
  stl_decomposition <- tryCatch({
    primary_mkt <- markets[1]
    front_primary <- front |>
      dplyr::filter(.data$market == primary_mkt) |>
      dplyr::arrange(.data$date)

    if (nrow(front_primary) < 365 * 2) stop("need 2y data for STL")

    ts_obj <- stats::ts(front_primary$value, frequency = 252)
    stl_fit <- stats::stl(ts_obj, s.window = "periodic")

    tibble::tibble(
      date = front_primary$date,
      market = primary_mkt,
      trend = as.numeric(stl_fit$time.series[, "trend"]),
      seasonal = as.numeric(stl_fit$time.series[, "seasonal"]),
      remainder = as.numeric(stl_fit$time.series[, "remainder"])
    )
  }, error = function(e) {
    tibble::tibble(date = as.Date(character()), market = character(),
                   trend = numeric(), seasonal = numeric(), remainder = numeric())
  })

  # --- seasonal_spreads: M1-M2 spread by day-of-year with percentile bands ---
  seasonal_spreads <- tryCatch({
    purrr::map_dfr(markets, function(mkt) {
      mkt_wide <- curves |>
        dplyr::filter(.data$market == mkt, .data$curve_point_num %in% c(1L, 2L)) |>
        dplyr::select(.data$date, .data$curve_point_num, .data$value) |>
        tidyr::pivot_wider(names_from = .data$curve_point_num, values_from = .data$value) |>
        dplyr::arrange(.data$date) |>
        dplyr::filter(!is.na(.data$`1`), !is.na(.data$`2`)) |>
        dplyr::mutate(
          spread = .data$`1` - .data$`2`,
          day_of_year = as.integer(format(.data$date, "%j")),
          year = as.integer(format(.data$date, "%Y"))
        )

      if (nrow(mkt_wide) < 30) return(tibble::tibble())

      hist_years <- mkt_wide |> dplyr::filter(.data$year < latest_year, .data$year >= latest_year - 5L)
      if (nrow(hist_years) < 30) return(tibble::tibble())

      hist_years |>
        dplyr::group_by(.data$day_of_year) |>
        dplyr::summarise(
          avg   = mean(.data$spread, na.rm = TRUE),
          p10   = stats::quantile(.data$spread, 0.10, na.rm = TRUE),
          p25   = stats::quantile(.data$spread, 0.25, na.rm = TRUE),
          p75   = stats::quantile(.data$spread, 0.75, na.rm = TRUE),
          p90   = stats::quantile(.data$spread, 0.90, na.rm = TRUE),
          .groups = "drop"
        ) |>
        dplyr::mutate(market = mkt)
    })
  }, error = function(e) {
    tibble::tibble(day_of_year = integer(), market = character(),
                   avg = numeric(), p10 = numeric(), p25 = numeric(), p75 = numeric(), p90 = numeric())
  })

  # --- seasonal_vol: realized vol by day-of-year with percentile bands ---
  seasonal_vol <- tryCatch({
    purrr::map_dfr(markets, function(mkt) {
      mkt_front <- front |>
        dplyr::filter(.data$market == mkt) |>
        dplyr::arrange(.data$date) |>
        dplyr::mutate(
          log_return = log(.data$value / dplyr::lag(.data$value)),
          day_of_year = as.integer(format(.data$date, "%j")),
          year = as.integer(format(.data$date, "%Y"))
        ) |>
        dplyr::filter(!is.na(.data$log_return), is.finite(.data$log_return))

      # Rolling 20-day realized vol
      mkt_front$rv20 <- slider::slide_dbl(
        mkt_front$log_return,
        ~ stats::sd(.x, na.rm = TRUE) * sqrt(252),
        .before = 19L, .complete = TRUE
      )

      hist_years <- mkt_front |> dplyr::filter(.data$year < latest_year, .data$year >= latest_year - 5L)
      if (nrow(hist_years) < 20) return(tibble::tibble())

      hist_years |>
        dplyr::filter(!is.na(.data$rv20)) |>
        dplyr::group_by(.data$day_of_year) |>
        dplyr::summarise(
          avg_vol = mean(.data$rv20, na.rm = TRUE),
          p25_vol = stats::quantile(.data$rv20, 0.25, na.rm = TRUE),
          p75_vol = stats::quantile(.data$rv20, 0.75, na.rm = TRUE),
          .groups = "drop"
        ) |>
        dplyr::mutate(market = mkt)
    })
  }, error = function(e) {
    tibble::tibble(day_of_year = integer(), market = character(),
                   avg_vol = numeric(), p25_vol = numeric(), p75_vol = numeric())
  })

  # --- seasonal_hedge_effectiveness: rolling R-squared by calendar month ---
  seasonal_hedge_effectiveness <- tryCatch({
    if (length(markets) < 2) stop("need at least 2 markets")
    wide_returns <- front |>
      dplyr::arrange(.data$market, .data$date) |>
      dplyr::group_by(.data$market) |>
      dplyr::mutate(log_return = log(.data$value / dplyr::lag(.data$value))) |>
      dplyr::ungroup() |>
      dplyr::filter(!is.na(.data$log_return)) |>
      dplyr::select("date", "market", "log_return") |>
      tidyr::pivot_wider(names_from = .data$market, values_from = .data$log_return) |>
      dplyr::arrange(.data$date) |>
      stats::na.omit()
    if (ncol(wide_returns) < 3) stop("need at least 2 market columns")
    x_col <- names(wide_returns)[2]
    y_cols <- names(wide_returns)[seq(3, ncol(wide_returns))]
    purrr::map_dfr(y_cols, function(y_col) {
      df_with_month <- wide_returns |>
        dplyr::mutate(month = format(.data$date, "%b"))
      purrr::map_dfr(month.abb, function(mon) {
        df_m <- df_with_month |> dplyr::filter(.data$month == mon)
        if (nrow(df_m) < 10) return(tibble::tibble(month = mon, r_squared = NA_real_, market = y_col))
        tryCatch({
          fit <- stats::lm(df_m[[y_col]] ~ df_m[[x_col]])
          tibble::tibble(month = mon, r_squared = broom::glance(fit)$r.squared, market = y_col)
        }, error = function(e) {
          tibble::tibble(month = mon, r_squared = NA_real_, market = y_col)
        })
      })
    })
  }, error = function(e) {
    tibble::tibble(month = character(), r_squared = numeric(), market = character())
  })

  # --- seasonal_summary: anomaly score, direction, days to inflection per market ---
  seasonal_summary <- purrr::map_dfr(markets, function(mkt) {
    dev <- seasonal_deviation |> dplyr::filter(.data$market == mkt)
    ret <- seasonal_returns |> dplyr::filter(.data$market == mkt)

    current_percentile <- if (nrow(dev) > 0) utils::tail(dev$percentile, 1L) else NA_real_
    anomaly_score <- abs(current_percentile - 0.5) * 2

    current_period <- format(Sys.Date(), "%b")
    next_month_idx <- (which(month.abb == current_period) %% 12L) + 1L
    next_month <- month.abb[next_month_idx]
    next_direction <- if (nrow(ret) > 0 && next_month %in% ret$period) {
      avg_r <- ret$avg_return[ret$period == next_month]
      if (length(avg_r) > 0 && !is.na(avg_r[1]) && avg_r[1] > 0) "up" else "down"
    } else {
      NA_character_
    }

    tibble::tibble(
      market = mkt,
      anomaly_score = anomaly_score,
      current_percentile = current_percentile,
      direction = next_direction,
      days_to_inflection = 30L
    )
  })

  # --- kpis: 5 rows ---
  primary_mkt <- markets[1]
  primary_dev <- seasonal_deviation |> dplyr::filter(.data$market == primary_mkt)
  primary_ret <- seasonal_returns |> dplyr::filter(.data$market == primary_mkt)
  primary_mr  <- mean_reversion |> dplyr::filter(.data$market == primary_mkt)
  primary_sum <- seasonal_summary |> dplyr::filter(.data$market == primary_mkt)

  best_month <- if (nrow(primary_ret) > 0) {
    primary_ret[which.max(primary_ret$avg_return), ]
  } else {
    NULL
  }

  kpis <- tibble::tribble(
    ~title, ~value, ~delta, ~status,
    "Current Percentile",
    if (nrow(primary_dev) > 0 && !is.na(utils::tail(primary_dev$percentile, 1L))) {
      scales::percent(utils::tail(primary_dev$percentile, 1L), accuracy = 1)
    } else { "N/A" },
    "vs 5yr range",
    if (nrow(primary_dev) > 0) {
      p <- utils::tail(primary_dev$percentile, 1L)
      if (!is.na(p) && (p > 0.75 || p < 0.25)) "warning" else "neutral"
    } else { "neutral" },

    "Best Month",
    if (!is.null(best_month)) best_month$period[1] else "N/A",
    if (!is.null(best_month)) scales::percent(best_month$avg_return[1], accuracy = 0.1) else "",
    "positive",

    "Anomaly Score",
    if (nrow(primary_sum) > 0 && !is.na(primary_sum$anomaly_score[1])) {
      scales::number(primary_sum$anomaly_score[1], accuracy = 0.01)
    } else { "N/A" },
    "0=normal, 1=extreme",
    if (nrow(primary_sum) > 0 && !is.na(primary_sum$anomaly_score[1]) && primary_sum$anomaly_score[1] > 0.5) {
      "warning"
    } else { "neutral" },

    "OU Half-Life",
    if (nrow(primary_mr) > 0 && !is.na(primary_mr$half_life[1])) {
      paste0(round(primary_mr$half_life[1]), "d")
    } else { "N/A" },
    "M1-M2 mean-rev",
    if (nrow(primary_mr) > 0 && !is.na(primary_mr$half_life[1]) && primary_mr$half_life[1] < 30) {
      "positive"
    } else { "neutral" },

    "Seasonal Direction",
    if (nrow(primary_sum) > 0 && !is.na(primary_sum$direction[1])) {
      toupper(primary_sum$direction[1])
    } else { "N/A" },
    "next month",
    if (nrow(primary_sum) > 0 && !is.na(primary_sum$direction[1]) && primary_sum$direction[1] == "up") {
      "positive"
    } else { "neutral" }
  )

  # --- notes and assumptions ---
  notes <- c(
    "Seasonal overlay indexed to 100 at the start of each year.",
    "STL decomposition uses stats::stl() with s.window='periodic' on daily front prices.",
    "OU mean-reversion fit via RTL::fitOU on the M1-M2 spread."
  )
  assumptions <- c(
    "Historical bands computed from 5 prior years.",
    "Realized vol uses a 20-day rolling window, annualized at sqrt(252).",
    "STL requires at least 2 years of daily price history."
  )

  list(
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
    notes = notes,
    assumptions = assumptions
  )
}
