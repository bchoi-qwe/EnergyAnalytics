# ---- Fundamentals Calculation Layer ----
# EIA inventory analytics from the canonical snapshot.

ea_market_to_eia_product <- function(markets) {
  mapping <- c(
    CL = "crude", BRN = "crude",
    HO = "distillate", RB = "gasoline", NG = "ng"
  )
  unique(mapping[markets[markets %in% names(mapping)]])
}

ea_calc_fundamentals <- function(filters) {
  markets <- ea_coalesce(filters$commodities, c("CL", "BRN", "RB"))
  products <- ea_market_to_eia_product(markets)

  stocks <- ea_load_dataset("eia_stocks_long") |>
    dplyr::filter(.data$product %in% products)

  capacity <- tryCatch(
    ea_load_dataset("eia_storage_capacity_long") |>
      dplyr::filter(.data$product %in% products),
    error = function(e) tibble::tibble()
  )

  # --- stocks_timeseries ---
  stocks_timeseries <- stocks |>
    dplyr::select("date", "product", "location", "value", "unit")

  # --- stocks_seasonal: current year vs 5-year range by week ---
  latest_year <- as.integer(format(max(stocks$date, na.rm = TRUE), "%Y"))
  five_year_start <- latest_year - 5L

  stocks_weekly <- stocks |>
    dplyr::mutate(
      year = as.integer(format(.data$date, "%Y")),
      week_num = as.integer(format(.data$date, "%V"))
    )

  historical <- stocks_weekly |>
    dplyr::filter(.data$year >= five_year_start, .data$year < latest_year) |>
    dplyr::group_by(.data$product, .data$location, .data$week_num) |>
    dplyr::summarise(
      avg_5yr = mean(.data$value, na.rm = TRUE),
      min_5yr = min(.data$value, na.rm = TRUE),
      max_5yr = max(.data$value, na.rm = TRUE),
      .groups = "drop"
    )

  current_year <- stocks_weekly |>
    dplyr::filter(.data$year == latest_year) |>
    dplyr::select("product", "location", "week_num", current = "value")

  stocks_seasonal <- historical |>
    dplyr::left_join(current_year, by = c("product", "location", "week_num")) |>
    dplyr::rename(week = "week_num") |>
    dplyr::arrange(.data$product, .data$week)

  # --- stocks_deviation ---
  stocks_with_dates <- stocks_weekly |>
    dplyr::filter(.data$year == latest_year) |>
    dplyr::select("date", "product", "location", "week_num")

  stocks_deviation <- stocks_seasonal |>
    dplyr::filter(!is.na(.data$current)) |>
    dplyr::mutate(
      deviation = .data$current - .data$avg_5yr,
      deviation_pct = (.data$current - .data$avg_5yr) / .data$avg_5yr
    ) |>
    dplyr::left_join(stocks_with_dates, by = c("product", "location", "week" = "week_num")) |>
    dplyr::select("date", "product", "deviation", "deviation_pct") |>
    dplyr::filter(!is.na(.data$date))

  # --- storage_capacity_util ---
  storage_capacity_util <- if (nrow(capacity) > 0) {
    latest_cap <- capacity |>
      dplyr::filter(.data$date == max(.data$date, na.rm = TRUE)) |>
      dplyr::select("product", "location", capacity = "value")

    latest_stocks <- stocks |>
      dplyr::group_by(.data$product, .data$location) |>
      dplyr::filter(.data$date == max(.data$date, na.rm = TRUE)) |>
      dplyr::ungroup() |>
      dplyr::select("date", "product", "location", stocks = "value")

    latest_stocks |>
      dplyr::inner_join(latest_cap, by = c("product", "location")) |>
      dplyr::mutate(utilization_pct = .data$stocks / .data$capacity) |>
      dplyr::select("date", "product", "capacity", "stocks", "utilization_pct")
  } else {
    tibble::tibble(
      date = as.Date(character()), product = character(),
      capacity = numeric(), stocks = numeric(), utilization_pct = numeric()
    )
  }

  list(
    stocks_timeseries = stocks_timeseries,
    stocks_seasonal = stocks_seasonal,
    stocks_deviation = stocks_deviation,
    storage_capacity_util = storage_capacity_util
  )
}
