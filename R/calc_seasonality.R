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
        mean(.data$values <= .data$current_val)
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

  list(
    seasonal_overlay = seasonal_overlay,
    seasonal_range = seasonal_range,
    seasonal_returns = seasonal_returns,
    mean_reversion = mean_reversion,
    seasonal_deviation = seasonal_deviation
  )
}
