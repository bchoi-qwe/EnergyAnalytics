# ---- Fundamentals Calculation Layer ----
# Inventory-focused EIA analytics from the canonical snapshot.

ea_inventory_market_spec <- function() {
  tibble::tribble(
    ~market, ~product, ~location, ~display_label, ~product_order,
    "CL", "crude", "Cushing", "CL • Crude • Cushing", 1L,
    "NG", "ng", "Lower48", "NG • Natural Gas • Lower48", 2L,
    "HO", "distillate", "US", "HO • Distillate • US", 3L,
    "RB", "gasoline", "US", "RB • Gasoline • US", 4L
  )
}

ea_inventory_market_map <- function() {
  spec <- ea_inventory_market_spec()
  stats::setNames(spec$product, spec$market)
}

ea_market_to_eia_product <- function(markets) {
  mapping <- ea_inventory_market_map()
  unique(unname(mapping[markets[markets %in% names(mapping)]]))
}

ea_inventory_product_label <- function(product) {
  labels <- c(
    crude = "Crude",
    distillate = "Distillate",
    gasoline = "Gasoline",
    ng = "Natural Gas"
  )

  unname(labels[[product]] %||% tools::toTitleCase(product))
}

ea_inventory_preferred_location <- function(locations) {
  if (length(locations) == 0L) {
    return(NA_character_)
  }

  location_lower <- tolower(locations)
  preferred_order <- c("total", "us", "lower48", "cushing")

  for (candidate in preferred_order) {
    idx <- match(candidate, location_lower, nomatch = 0L)
    if (idx > 0L) {
      return(locations[[idx]])
    }
  }

  locations[[1]]
}

ea_inventory_display_window <- function(filters, available_dates = NULL) {
  if (!is.null(filters$date_range) && length(filters$date_range) == 2L) {
    return(range(as.Date(filters$date_range)))
  }

  valid_dates <- available_dates[!is.na(available_dates)]
  if (length(valid_dates) == 0L) {
    return(c(Sys.Date() - 365L, Sys.Date()))
  }

  max_date <- max(valid_dates)
  c(max_date - 365L, max_date)
}

ea_calc_fundamentals <- function(filters) {
  catalog <- ea_market_catalog()
  inventory_spec <- ea_inventory_market_spec()
  markets <- ea_coalesce(filters$commodities, catalog$market)
  unsupported_markets <- setdiff(markets, inventory_spec$market)
  inventory_markets <- inventory_spec %>%
    dplyr::filter(.data$market %in% markets) %>%
    dplyr::arrange(.data$product_order)
  products <- unique(inventory_markets$product)

  empty_inventory <- tibble::tibble(
    date = as.Date(character()),
    product = character(),
    location = character(),
    current = numeric(),
    avg_5yr = numeric(),
    min_5yr = numeric(),
    max_5yr = numeric(),
    deviation = numeric(),
    deviation_pct = numeric(),
    unit = character(),
    raw_unit = character(),
    iso_year = integer(),
    iso_week = integer(),
    observations_5yr = integer()
  )

  empty_context <- list(
    market = NA_character_,
    market_label = NA_character_,
    product = NA_character_,
    location = NA_character_,
    unit = NA_character_
  )

  empty_markets <- tibble::tibble(
    market = character(),
    product = character(),
    location = character(),
    unit = character(),
    display_label = character(),
    product_order = integer()
  )

  if (length(products) == 0L) {
    return(list(
      inventory_history = empty_inventory,
      inventory_context = empty_context,
      inventory_markets = empty_markets,
      inventory_supported_markets = character(),
      inventory_unsupported_markets = unsupported_markets,
      stocks_timeseries = empty_inventory,
      stocks_deviation = empty_inventory %>%
        dplyr::select("date", "product", "location", "deviation", "deviation_pct"),
      notes = c(
        "Inventory data comes from the packaged EIA snapshot.",
        "Only inventory-backed products are shown on this page."
      ),
      assumptions = c(
        "The selected date range controls the chart window.",
        "The 5-year baseline compares each ISO week to the prior five completed ISO years."
      )
    ))
  }

  stocks <- ea_load_dataset("eia_stocks_long") %>%
    dplyr::filter(.data$product %in% products) %>%
    dplyr::arrange(.data$product, .data$location, .data$date)

  display_window <- ea_inventory_display_window(filters, stocks$date)

  stocks_weekly <- stocks %>%
    dplyr::mutate(
      iso_year = as.integer(format(.data$date, "%G")),
      iso_week = as.integer(format(.data$date, "%V"))
    )

  reference_history <- stocks_weekly %>%
    dplyr::select(
      "product",
      "location",
      "iso_week",
      hist_iso_year = "iso_year",
      hist_value = "value"
    )

  stocks_with_reference <- stocks_weekly %>%
    dplyr::left_join(
      reference_history,
      by = c("product", "location", "iso_week"),
      relationship = "many-to-many"
    ) %>%
    dplyr::filter(
      is.na(.data$hist_iso_year) |
        (
          .data$hist_iso_year >= (.data$iso_year - 5L) &
            .data$hist_iso_year < .data$iso_year
        )
    ) %>%
    dplyr::group_by(
      .data$date,
      .data$product,
      .data$location,
      .data$value,
      .data$unit,
      .data$raw_unit,
      .data$iso_year,
      .data$iso_week
    ) %>%
    dplyr::summarise(
      avg_5yr = if (all(is.na(.data$hist_value))) NA_real_ else mean(.data$hist_value, na.rm = TRUE),
      min_5yr = if (all(is.na(.data$hist_value))) NA_real_ else min(.data$hist_value, na.rm = TRUE),
      max_5yr = if (all(is.na(.data$hist_value))) NA_real_ else max(.data$hist_value, na.rm = TRUE),
      observations_5yr = sum(!is.na(.data$hist_value)),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      current = .data$value,
      deviation = .data$current - .data$avg_5yr,
      deviation_pct = dplyr::if_else(
        !is.na(.data$avg_5yr) & .data$avg_5yr != 0,
        .data$deviation / .data$avg_5yr,
        NA_real_
      )
    ) %>%
    dplyr::arrange(.data$product, .data$location, .data$date)

  latest_inventory_levels <- stocks_with_reference %>%
    dplyr::group_by(.data$product, .data$location) %>%
    dplyr::filter(.data$date == max(.data$date, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(
      product = .data$product,
      inventory_location = .data$location,
      inventory_date = .data$date,
      current = .data$current,
      inventory_unit = .data$unit
    )

  storage_capacity <- ea_load_dataset("eia_storage_capacity_long") %>%
    dplyr::filter(.data$product %in% products) %>%
    dplyr::group_by(.data$product, .data$location) %>%
    dplyr::filter(.data$date == max(.data$date, na.rm = TRUE)) %>%
    dplyr::ungroup()

  inventory_markets <- inventory_markets %>%
    dplyr::inner_join(
      stocks_with_reference %>%
        dplyr::distinct(.data$product, .data$location, .data$unit),
      by = c("product", "location")
    ) %>%
    dplyr::arrange(.data$product_order)

  inventory_history <- stocks_with_reference %>%
    dplyr::inner_join(
      inventory_markets %>%
        dplyr::select("market", "product", "location", "display_label", "product_order"),
      by = c("product", "location")
    ) %>%
    dplyr::filter(
      .data$date >= display_window[1],
      .data$date <= display_window[2]
    ) %>%
    dplyr::arrange(.data$product_order, .data$date)

  primary_product_row <- inventory_markets %>%
    dplyr::arrange(.data$product_order) %>%
    dplyr::slice(1L)

  inventory_context <- if (nrow(primary_product_row) > 0L) {
    list(
      market = primary_product_row$market[[1]],
      market_label = ea_market_labels(primary_product_row$market[[1]], catalog)[1],
      product = primary_product_row$product[[1]],
      location = primary_product_row$location[[1]],
      unit = primary_product_row$unit[[1]]
    )
  } else {
    empty_context
  }

  stocks_timeseries <- inventory_history

  stocks_seasonal <- inventory_history %>%
    dplyr::transmute(
      date = .data$date,
      week = .data$iso_week,
      market = .data$market,
      product = .data$product,
      location = .data$location,
      current = .data$current,
      avg_5yr = .data$avg_5yr,
      min_5yr = .data$min_5yr,
      max_5yr = .data$max_5yr
    )

  stocks_deviation <- stocks_timeseries %>%
    dplyr::select("date", "product", "location", "deviation", "deviation_pct")

  storage_capacity_util <- purrr::pmap_dfr(
    inventory_markets,
    function(market, product, location, display_label, product_order, unit) {
      product_capacity <- storage_capacity %>%
        dplyr::filter(.data$product == product)

      capacity_location <- if (nrow(product_capacity) == 0L) {
        NA_character_
      } else if (location %in% product_capacity$location) {
        location
      } else {
        ea_inventory_preferred_location(product_capacity$location)
      }

      tibble::tibble(
        market = market,
        product = product,
        location = location,
        capacity_location = capacity_location
      )
    }
  ) %>%
    dplyr::left_join(
      latest_inventory_levels,
      by = c("product", "location" = "inventory_location")
    ) %>%
    dplyr::left_join(
      storage_capacity %>%
        dplyr::transmute(
          .data$product,
          capacity_location = .data$location,
          capacity_date = .data$date,
          capacity = .data$value,
          capacity_unit = .data$unit
        ),
      by = c("product", "capacity_location")
  ) %>%
    dplyr::mutate(
      utilization_pct = dplyr::if_else(
        !is.na(.data$current) & !is.na(.data$capacity) & .data$capacity != 0,
        .data$current / .data$capacity,
        NA_real_
      )
    ) %>%
    dplyr::transmute(
      market = .data$market,
      product = .data$product,
      inventory_location = .data$location,
      capacity_location = .data$capacity_location,
      current = .data$current,
      capacity = .data$capacity,
      utilization_pct = .data$utilization_pct,
      inventory_date = .data$inventory_date,
      capacity_date = .data$capacity_date,
      inventory_unit = .data$inventory_unit,
      capacity_unit = .data$capacity_unit
    )

  latest_focus <- stocks_timeseries %>%
    dplyr::filter(.data$market == inventory_context$market) %>%
    dplyr::arrange(.data$date) %>%
    dplyr::slice_tail(n = 1L)

  focus_util <- storage_capacity_util %>%
    dplyr::filter(.data$market == inventory_context$market) %>%
    dplyr::slice_head(n = 1L)

  kpis <- if (nrow(latest_focus) == 0L) {
    tibble::tibble(
      title = character(),
      value = character(),
      delta = character(),
      status = character()
    )
  } else {
    tibble::tribble(
      ~title, ~value, ~delta, ~status,
      "Inventory",
      scales::comma(latest_focus$current[[1]]),
      latest_focus$location[[1]],
      "neutral",
      "Gap vs 5Y",
      if (is.finite(latest_focus$deviation[[1]])) scales::comma(latest_focus$deviation[[1]]) else "N/A",
      latest_focus$product[[1]],
      "neutral",
      "Gap Percent",
      if (is.finite(latest_focus$deviation_pct[[1]])) scales::percent(latest_focus$deviation_pct[[1]], accuracy = 0.1) else "N/A",
      "vs seasonal baseline",
      "neutral",
      "Utilization",
      if (nrow(focus_util) > 0L && is.finite(focus_util$utilization_pct[[1]])) scales::percent(focus_util$utilization_pct[[1]], accuracy = 0.1) else "N/A",
      if (nrow(focus_util) > 0L && !is.na(focus_util$capacity_location[[1]])) focus_util$capacity_location[[1]] else "capacity unavailable",
      "neutral"
    )
  }

  notes <- c(
    "Inventory charts use the packaged EIA weekly inventory snapshot.",
    "Storage capacity utilization uses the packaged EIA capacity snapshot when a compatible product/location series is available.",
    "The chart window follows the shared date filter."
  )

  assumptions <- c(
    "The 5-year baseline compares each observation's ISO week with the prior five completed ISO years.",
    "If fewer than five historical observations exist for a week, the baseline uses the available history only.",
    "Capacity fallback uses the closest available product-level location when an exact inventory location is not published."
  )

  list(
    inventory_history = inventory_history,
    inventory_context = inventory_context,
    inventory_markets = inventory_markets,
    inventory_supported_markets = inventory_markets$market,
    inventory_unsupported_markets = unsupported_markets,
    stocks_timeseries = stocks_timeseries,
    stocks_seasonal = stocks_seasonal,
    stocks_deviation = stocks_deviation,
    storage_capacity_util = storage_capacity_util,
    kpis = kpis,
    notes = notes,
    assumptions = assumptions
  )
}
