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

  # --- crack_spreads ---
  crack_spreads <- tryCatch({
    curves <- ea_load_dataset("commodity_curve_long") |>
      dplyr::filter(.data$curve_point_num == 1L) |>
      dplyr::select(.data$date, .data$market, .data$value)

    crack_defs <- list(
      "RB Crack (RB-CL)" = c("RB", "CL"),
      "HO Crack (HO-CL)" = c("HO", "CL")
    )

    available_mkts <- unique(curves$market)

    purrr::map_dfr(names(crack_defs), function(crack_name) {
      legs <- crack_defs[[crack_name]]
      if (!all(legs %in% available_mkts)) return(tibble::tibble())

      wide <- curves |>
        dplyr::filter(.data$market %in% legs) |>
        dplyr::select(.data$date, .data$market, .data$value) |>
        tidyr::pivot_wider(names_from = .data$market, values_from = .data$value) |>
        dplyr::arrange(.data$date) |>
        dplyr::filter(!is.na(.data[[legs[1]]]), !is.na(.data[[legs[2]]]))

      if (nrow(wide) == 0) return(tibble::tibble())

      wide |>
        dplyr::mutate(
          crack_value = .data[[legs[1]]] - .data[[legs[2]]],
          spread_label = crack_name
        ) |>
        dplyr::select(.data$date, .data$spread_label, crack_value = .data$crack_value)
    })
  }, error = function(e) {
    tibble::tibble(date = as.Date(character()), spread_label = character(), crack_value = numeric())
  })

  # --- treasury_snapshot ---
  treasury_snapshot <- tryCatch({
    ust <- ea_load_dataset("ust_curve_long")
    all_dates <- sort(unique(ust$date), decreasing = TRUE)
    if (length(all_dates) < 1) stop("no UST data")

    target_dates <- c(
      current = all_dates[1],
      m1_ago = all_dates[pmin(22L, length(all_dates))],
      m3_ago = all_dates[pmin(63L, length(all_dates))]
    )

    purrr::map_dfr(names(target_dates), function(label) {
      d <- target_dates[[label]]
      ust |>
        dplyr::filter(.data$date == d) |>
        dplyr::select(.data$curve_point_num, yield = .data$value) |>
        dplyr::mutate(snapshot_label = label)
    })
  }, error = function(e) {
    tibble::tibble(curve_point_num = numeric(), yield = numeric(), snapshot_label = character())
  })

  # --- commodity_rate_scatter ---
  commodity_rate_scatter <- tryCatch({
    ust <- ea_load_dataset("ust_curve_long")
    curves_all <- ea_load_dataset("commodity_curve_long") |>
      dplyr::filter(.data$curve_point_num == 1L, .data$market %in% markets) |>
      dplyr::arrange(.data$market, .data$date) |>
      dplyr::group_by(.data$market) |>
      dplyr::mutate(commodity_chg = suppressWarnings(log(.data$value / dplyr::lag(.data$value)))) |>
      dplyr::ungroup() |>
      dplyr::filter(!is.na(.data$commodity_chg), is.finite(.data$commodity_chg))

    ust_10y <- ust |>
      dplyr::filter(.data$curve_point_num == 10) |>
      dplyr::arrange(.data$date) |>
      dplyr::mutate(rate_chg = .data$value - dplyr::lag(.data$value)) |>
      dplyr::filter(!is.na(.data$rate_chg)) |>
      dplyr::select(.data$date, rate_chg = .data$rate_chg)

    curves_all |>
      dplyr::inner_join(ust_10y, by = "date") |>
      dplyr::select(.data$date, .data$market, .data$commodity_chg, .data$rate_chg)
  }, error = function(e) {
    tibble::tibble(date = as.Date(character()), market = character(),
                   commodity_chg = numeric(), rate_chg = numeric())
  })

  # --- release_calendar ---
  release_calendar <- {
    today <- Sys.Date()
    # EIA WPSR: every Wednesday; EIA Gas Storage: every Thursday
    next_dates <- function(weekday_num, n = 8L) {
      days_ahead <- (weekday_num - as.integer(format(today, "%u"))) %% 7L
      if (days_ahead == 0L) days_ahead <- 7L
      start <- today + days_ahead
      seq(start, by = 7L, length.out = n)
    }

    wednesdays <- next_dates(3L)  # Wednesday = 3 in ISO
    thursdays  <- next_dates(4L)  # Thursday = 4 in ISO

    tibble::tibble(
      release_date = c(wednesdays, thursdays),
      event = c(
        rep("EIA Weekly Petroleum Status Report (WPSR)", length(wednesdays)),
        rep("EIA Natural Gas Storage Report", length(thursdays))
      ),
      day_of_week = c(
        rep("Wednesday", length(wednesdays)),
        rep("Thursday", length(thursdays))
      )
    ) |>
      dplyr::arrange(.data$release_date)
  }

  # --- kpis ---
  # Latest weekly stocks change
  latest_dev <- stocks_deviation |>
    dplyr::arrange(dplyr::desc(.data$date)) |>
    dplyr::slice(1L)

  # Latest crack spread
  latest_crack <- if (nrow(crack_spreads) > 0) {
    crack_spreads |>
      dplyr::arrange(dplyr::desc(.data$date)) |>
      dplyr::group_by(.data$spread_label) |>
      dplyr::slice(1L) |>
      dplyr::ungroup() |>
      dplyr::slice(1L)
  } else {
    NULL
  }

  # Utilization
  top_util <- storage_capacity_util |>
    dplyr::slice_max(.data$utilization_pct, n = 1L, with_ties = FALSE)

  kpis <- tibble::tribble(
    ~title, ~value, ~delta, ~status,

    "Inventory Deviation",
    if (nrow(latest_dev) > 0 && !is.na(latest_dev$deviation[1])) {
      scales::comma(latest_dev$deviation[1], accuracy = 1)
    } else { "N/A" },
    if (nrow(latest_dev) > 0 && !is.na(latest_dev$deviation_pct[1])) {
      scales::percent(latest_dev$deviation_pct[1], accuracy = 0.1)
    } else { "vs 5yr avg" },
    if (nrow(latest_dev) > 0 && !is.na(latest_dev$deviation_pct[1])) {
      if (abs(latest_dev$deviation_pct[1]) > 0.10) "warning" else "neutral"
    } else { "neutral" },

    "Best Crack Spread",
    if (!is.null(latest_crack) && nrow(latest_crack) > 0) {
      scales::number(latest_crack$crack_value[1], accuracy = 0.01)
    } else { "N/A" },
    if (!is.null(latest_crack) && nrow(latest_crack) > 0) {
      latest_crack$spread_label[1]
    } else { "" },
    "positive",

    "Storage Utilization",
    if (nrow(top_util) > 0 && !is.na(top_util$utilization_pct[1])) {
      scales::percent(top_util$utilization_pct[1], accuracy = 1)
    } else { "N/A" },
    if (nrow(top_util) > 0) top_util$product[1] else "",
    if (nrow(top_util) > 0 && !is.na(top_util$utilization_pct[1]) && top_util$utilization_pct[1] > 0.85) "warning" else "neutral",

    "Next Release",
    if (nrow(release_calendar) > 0) {
      format(release_calendar$release_date[1], "%d %b")
    } else { "N/A" },
    if (nrow(release_calendar) > 0) release_calendar$event[1] else "",
    "neutral",

    "UST 10Y",
    if (nrow(treasury_snapshot) > 0) {
      curr <- treasury_snapshot |> dplyr::filter(.data$snapshot_label == "current", .data$curve_point_num == 10)
      if (nrow(curr) > 0) scales::number(curr$yield[1], accuracy = 0.01) else "N/A"
    } else { "N/A" },
    "current yield",
    "neutral"
  )

  # --- notes and assumptions ---
  notes <- c(
    "EIA inventory data sourced from the canonical snapshot via RTL::eia2tidy_all.",
    "Crack spreads computed from front contract prices (RB-CL, HO-CL).",
    "Treasury snapshot shows current, 1-month, and 3-month ago yield curves."
  )
  assumptions <- c(
    "Inventory 5-year range uses the 5 complete calendar years preceding the most recent year.",
    "WPSR and gas storage release dates are deterministic (Wednesday/Thursday schedule).",
    "Commodity-rate scatter uses 10Y UST as the interest rate proxy."
  )

  list(
    stocks_timeseries = stocks_timeseries,
    stocks_seasonal = stocks_seasonal,
    stocks_deviation = stocks_deviation,
    storage_capacity_util = storage_capacity_util,
    crack_spreads = crack_spreads,
    treasury_snapshot = treasury_snapshot,
    commodity_rate_scatter = commodity_rate_scatter,
    release_calendar = release_calendar,
    kpis = kpis,
    notes = notes,
    assumptions = assumptions
  )
}
