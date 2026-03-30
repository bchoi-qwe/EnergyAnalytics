ea_market_catalog <- function() {
  required_markets <- tibble::tribble(
    ~market, ~label, ~complex, ~default_focus_order,
    "CL", "CL", "Crude Oil", 1L,
    "NG", "NG", "Natural Gas", 2L,
    "HTT", "HTT", "Natural Gas", 3L,
    "BRN", "BRN", "Crude Oil", 4L,
    "HO", "HO", "Refined Products", 5L,
    "RB", "RB", "Refined Products", 6L
  )

  registry <- tryCatch(
    ea_load_dataset("market_registry"),
    error = function(...) NULL
  )

  if (is.null(registry)) {
    registry <- required_markets
  }

  if (!"label" %in% names(registry)) registry$label <- registry$market
  if (!"complex" %in% names(registry)) registry$complex <- "Cross-Commodity"
  if (!"default_focus_order" %in% names(registry)) registry$default_focus_order <- seq_len(nrow(registry))

  missing_markets <- dplyr::anti_join(required_markets, registry, by = "market")
  registry <- dplyr::bind_rows(registry, missing_markets)

  registry |>
    dplyr::filter(.data$market %in% required_markets$market) |>
    dplyr::mutate(
      label = required_markets$label[match(.data$market, required_markets$market)],
      complex = dplyr::coalesce(required_markets$complex[match(.data$market, required_markets$market)], .data$complex),
      focus_order = dplyr::coalesce(required_markets$default_focus_order[match(.data$market, required_markets$market)], .data$default_focus_order, dplyr::row_number())
    ) |>
    dplyr::arrange(.data$focus_order) |>
    dplyr::select(dplyr::all_of(c("market", "label", "complex")))
}

ea_dashboard_timestamp <- function() {
  manifest <- tryCatch(
    ea_load_manifest(),
    error = function(...) NULL
  )

  built_at <- if (!is.null(manifest)) manifest$built_at_utc else NULL

  if (is.null(built_at) || !nzchar(built_at)) {
    return(Sys.time())
  }

  lubridate::with_tz(
    as.POSIXct(built_at, tz = "UTC"),
    tzone = ea_coalesce(Sys.timezone(), "UTC")
  )
}

ea_reference_end_date <- function() {
  manifest <- tryCatch(
    ea_load_manifest(),
    error = function(...) NULL
  )

  max_date <- if (!is.null(manifest)) manifest$datasets$commodity_curve_long$max_date else NULL
  as.Date(ea_coalesce(max_date, Sys.Date()))
}

ea_available_date_bounds <- function() {
  manifest <- tryCatch(
    ea_load_manifest(),
    error = function(...) NULL
  )

  min_date <- if (!is.null(manifest)) manifest$datasets$commodity_curve_long$min_date else NULL
  max_date <- if (!is.null(manifest)) manifest$datasets$commodity_curve_long$max_date else NULL

  c(
    as.Date(ea_coalesce(min_date, Sys.Date() - 365)),
    as.Date(ea_coalesce(max_date, Sys.Date()))
  )
}

ea_global_filter_defaults <- function(catalog = ea_market_catalog()) {
  date_bounds <- ea_available_date_bounds()

  list(
    market_complex = "Cross-Commodity",
    commodities = c("CL", "BRN", "RB"),
    comparison_commodity = "NG",
    date_range = c(max(date_bounds[2] - 365, date_bounds[1]), date_bounds[2]),
    rolling_window = "63D",
    tenor_bucket = c("Front", "Quarterly"),
    expiry_range = c(1, 12),
    regime = c("All Regimes"),
    scenario_preset = "Base Case"
  )
}

ea_default_comparison <- function(commodities, catalog = ea_market_catalog()) {
  candidates <- setdiff(catalog$market, commodities)
  if (length(candidates) == 0L) {
    return(ea_coalesce(commodities[[1]], catalog$market[[1]]))
  }

  candidates[[1]]
}

ea_lookup_value <- function(values, key) {
  unname(values[as.character(key)][[1]])
}

ea_market_index <- function(market) {
  match(market, names(ea_market_palette()))
}

ea_market_base_price <- function(market) {
  ea_lookup_value(c(CL = 74.25, NG = 3.18, HTT = 11.85, BRN = 77.10, HO = 2.34, RB = 2.23), market)
}

ea_market_base_vol <- function(market) {
  ea_lookup_value(c(CL = 0.31, NG = 0.47, HTT = 0.44, BRN = 0.29, HO = 0.32, RB = 0.34), market)
}

ea_market_curve_slope <- function(market) {
  ea_lookup_value(c(CL = 0.22, NG = -0.055, HTT = -0.038, BRN = 0.18, HO = 0.014, RB = 0.012), market)
}

ea_dates <- function(n = 180L, end_date = ea_reference_end_date()) {
  seq.Date(as.Date(end_date) - (n - 1L), as.Date(end_date), by = "day")
}

ea_curve_points <- function(n = 24L) {
  seq_len(n)
}

ea_mock_overview_data <- function(filters) {
  catalog <- ea_market_catalog()
  markets <- ea_coalesce(filters$commodities, ea_global_filter_defaults(catalog)$commodities)
  comparison <- ea_coalesce(filters$comparison_commodity, ea_default_comparison(markets, catalog))
  dates <- ea_dates(180)
  palette <- ea_market_palette()
  labels <- stats::setNames(catalog$label, catalog$market)

  rel_perf <- purrr::map_dfr(markets, function(market) {
    idx <- ea_market_index(market)
    steps <- seq_along(dates)
    level <- cumsum(sin(steps / 16 + idx) * 0.18 + cos(steps / 9 + idx / 3) * 0.07)
    tibble::tibble(
      date = dates,
      market = market,
      label = ea_lookup_value(labels, market),
      value = level - level[[1]]
    )
  })

  matrix_markets <- unique(c(markets, comparison))
  correlation_tbl <- expand.grid(x = matrix_markets, y = matrix_markets, stringsAsFactors = FALSE) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      value = purrr::map2_dbl(.data$x, .data$y, function(lhs, rhs) {
        if (identical(lhs, rhs)) {
          return(1)
        }

        lhs_idx <- ea_market_index(lhs)
        rhs_idx <- ea_market_index(rhs)
        round(max(min(0.78 - abs(lhs_idx - rhs_idx) * 0.19, 0.95), -0.45), 2)
      }),
      x_label = labels[.data$x],
      y_label = labels[.data$y]
    )

  primary <- markets[[1]]
  primary_idx <- ea_market_index(primary)
  comparison_idx <- ea_market_index(comparison)

  spread_monitor <- tibble::tibble(
    tenor = c("M1-M2", "M1-M6", "Q1-Q2", "CL-BRN", "RB-HO"),
    level = c(0.62, 1.84, 2.21, -1.17, 0.06) + primary_idx * 0.08,
    move = c(0.11, -0.08, 0.17, 0.24, -0.03) + comparison_idx * 0.01
  )

  kpis <- tibble::tribble(
    ~title, ~value, ~delta, ~icon, ~status,
    "Front-month change", scales::percent(0.009 + primary_idx * 0.0025, accuracy = 0.01), "vs prior settle", "chart-line", "positive",
    "M1/M6 slope", paste0(scales::number(1.1 + primary_idx * 0.25, accuracy = 0.01), " rel"), "front vs mid curve", "sliders-h", "neutral",
    "ATMF vol", scales::percent(0.25 + primary_idx * 0.035, accuracy = 0.1), "front contract", "wave-square", "warning",
    "Intercommodity spread", paste0(scales::number(-1.05 + comparison_idx * 0.14, accuracy = 0.01), " sigma"), "vs benchmark", "arrows-left-right", "neutral",
    "Cross-hedge ratio", scales::number(0.68 + comparison_idx * 0.06, accuracy = 0.01), "current beta", "shield-alt", "positive",
    "Market regime", "Seasonal carry", "current state", "calendar-day", "neutral"
  )

  detail_table <- tibble::tibble(
    Product = labels[markets],
    `Last price` = purrr::map_chr(markets, ~ scales::number(ea_market_base_price(.x), accuracy = 0.01)),
    `5D change` = purrr::map_chr(seq_along(markets), ~ scales::percent(0.004 + .x * 0.005, accuracy = 0.01)),
    `Curve slope` = purrr::map_chr(seq_along(markets), ~ paste0(scales::number(-0.3 + .x * 0.21, accuracy = 0.01), " rel")),
    `ATMF vol` = purrr::map_chr(markets, ~ scales::percent(ea_market_base_vol(.x), accuracy = 0.1)),
    `Cross-hedge beta` = purrr::map_chr(seq_along(markets), ~ scales::number(0.62 + .x * 0.11, accuracy = 0.01)),
    `Desk bias` = c("Own gamma", "Watch cracks", "Cross-hedge viable")[((seq_along(markets) - 1L) %% 3L) + 1L]
  )

  commentary <- list(
    title = paste(ea_lookup_value(labels, primary), "is leading price discovery in the selected complex."),
    body = glue::glue(
      "{ea_lookup_value(labels, primary)} has led the {filters$rolling_window} lookback while ",
      "{ea_lookup_value(labels, comparison)} remains the cleaner cross-hedge benchmark for relative-value and basis framing."
    ),
    bullets = c(
      "Front calendar spreads are steeper than the back curve, so the primary signal is curve shape rather than outright price.",
      "Cross-product correlation is still high enough for hedge discussions, but proxy products are decoupling at the short end.",
      "Seasonal structure remains relevant, so carry and cross-hedge conversations should be read together."
    )
  )

  list(
    kpis = kpis,
    rel_perf = rel_perf,
    correlation = correlation_tbl,
    spread_monitor = spread_monitor,
    detail_table = detail_table,
    commentary = commentary,
    notes = c(
      "Market Monitor charts are UI placeholders driven by deterministic mock data.",
      "Tiles and tables are designed to accept backend metrics without changing layout contracts."
    ),
    assumptions = c(
      "Values are synthetic and should not be used as trading signals.",
      "Selected products drive all cards through a single shared filters object."
    )
  )
}

ea_mock_forward_curve_data <- function(filters) {
  catalog <- ea_market_catalog()
  markets <- ea_coalesce(filters$commodities, ea_global_filter_defaults(catalog)$commodities[1:2])
  labels <- stats::setNames(catalog$label, catalog$market)
  tenors <- ea_curve_points(24)
  heatmap_dates <- ea_dates(36)

  curve_snapshot <- purrr::map_dfr(markets, function(market) {
    idx <- ea_market_index(market)
    tibble::tibble(
      market = market,
      label = ea_lookup_value(labels, market),
      tenor = tenors,
      price = ea_market_base_price(market) +
        ea_market_curve_slope(market) * tenors +
        sin((tenors + idx) / 3) * (idx * 0.09)
    )
  })

  primary <- markets[[1]]
  historical_compare <- expand.grid(
    tenor = tenors,
    snapshot = c("Latest", "1M Ago", "3M Ago"),
    stringsAsFactors = FALSE
  ) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      market = primary,
      label = ea_lookup_value(labels, primary),
      shift = dplyr::case_when(
        .data$snapshot == "Latest" ~ 0,
        .data$snapshot == "1M Ago" ~ -0.65,
        TRUE ~ -1.10
      ),
      price = ea_market_base_price(primary) +
        ea_market_curve_slope(primary) * .data$tenor +
        sin((.data$tenor + ea_market_index(primary)) / 3) * 0.18 +
        .data$shift
    )

  curve_heatmap <- expand.grid(
    date = heatmap_dates,
    tenor = seq_len(12),
    stringsAsFactors = FALSE
  ) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      value = sin(as.numeric(.data$date) / 14 + .data$tenor / 4) * 0.8 +
        cos(.data$tenor / 2) * 0.3
    )

  spread_structure <- tibble::tibble(
    spread = c("M1-M3", "M3-M6", "Q1-Q2", "Summer-Winter", "CL-BRN"),
    level = c(0.82, 0.57, 1.13, 1.54, -1.06),
    zscore = c(0.4, -0.3, 0.8, 1.2, -0.7)
  )

  list(
    curve_snapshot = curve_snapshot,
    historical_compare = historical_compare,
    curve_heatmap = curve_heatmap,
    spread_structure = spread_structure,
    notes = c(
      "Term structure layouts prioritize curve shape, curve history, and spread decomposition.",
      "Companion controls are local to the page and intentionally separate from global filters."
    ),
    assumptions = c(
      "Historical overlays are deterministic placeholders.",
      "Benchmark curve and tenor-set controls are ready for backend wiring."
    )
  )
}

ea_mock_volatility_data <- function(filters) {
  catalog <- ea_market_catalog()
  markets <- ea_coalesce(filters$commodities, ea_global_filter_defaults(catalog)$commodities[1:2])
  labels <- stats::setNames(catalog$label, catalog$market)
  tenors <- ea_curve_points(18)
  dates <- ea_dates(180)
  primary <- markets[[1]]

  kpis <- tibble::tribble(
    ~title, ~value, ~delta, ~icon, ~status,
    "Realized vol", scales::percent(ea_market_base_vol(primary) - 0.05, accuracy = 0.1), "63D lookback", "activity", "neutral",
    "ATMF vol", scales::percent(ea_market_base_vol(primary), accuracy = 0.1), "front strip", "chart-area", "warning",
    "Vol spread", paste0(scales::number(2.4 + ea_market_index(primary) * 0.3, accuracy = 0.1), " pts"), "implied minus realized", "arrows-left-right", "neutral",
    "Vol regime", "Elevated", "term dispersion", "gauge-high", "warning"
  )

  vol_term <- purrr::map_dfr(markets, function(market) {
    idx <- ea_market_index(market)
    tibble::tibble(
      market = market,
      label = ea_lookup_value(labels, market),
      tenor = tenors,
      vol = ea_market_base_vol(market) + log1p(tenors) * 0.012 + idx * 0.005
    )
  })

  vol_time <- purrr::map_dfr(markets, function(market) {
    idx <- ea_market_index(market)
    steps <- seq_along(dates)
    tibble::tibble(
      date = dates,
      market = market,
      label = ea_lookup_value(labels, market),
      vol = ea_market_base_vol(market) + sin(steps / 18 + idx) * 0.045 + cos(steps / 32) * 0.018
    )
  })

  vol_surface <- expand.grid(
    date = utils::tail(dates, 60),
    tenor = seq_len(12),
    stringsAsFactors = FALSE
  ) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      vol = ea_market_base_vol(primary) +
        sin(as.numeric(.data$date) / 25 + .data$tenor / 3) * 0.05 +
        .data$tenor * 0.004
    )

  skew <- expand.grid(
    moneyness = c(-0.15, -0.10, -0.05, 0, 0.05, 0.10, 0.15),
    market = markets,
    stringsAsFactors = FALSE
  ) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      label = labels[.data$market],
      vol = ea_market_base_vol(.data$market) + abs(.data$moneyness) * 0.35 + ea_market_index(.data$market) * 0.01
    )

  list(
    kpis = kpis,
    vol_term = vol_term,
    vol_time = vol_time,
    vol_surface = vol_surface,
    skew = skew,
    notes = c(
      "Vol Surface placeholders reserve space for term structure, history, and surface views.",
      "Legends and axis treatments stay compact for multi-product comparisons."
    ),
    assumptions = c(
      "Surface data is synthetic.",
      "ATMF, smile, and realized series will be replaced by backend estimators."
    )
  )
}

ea_mock_codynamics_data <- function(filters) {
  catalog <- ea_market_catalog()
  labels <- stats::setNames(catalog$label, catalog$market)
  markets <- ea_coalesce(filters$commodities, ea_global_filter_defaults(catalog)$commodities[1:2])
  comparison <- ea_coalesce(filters$comparison_commodity, ea_default_comparison(markets, catalog))
  focus_markets <- unique(c(markets, comparison))
  dates <- ea_dates(180)
  pairs <- utils::combn(focus_markets, 2, simplify = FALSE)

  rolling_corr <- purrr::map_dfr(pairs, function(pair) {
    lhs <- pair[[1]]
    rhs <- pair[[2]]
    steps <- seq_along(dates)
    tibble::tibble(
      date = dates,
      pair = paste(ea_lookup_value(labels, lhs), "vs", ea_lookup_value(labels, rhs)),
      value = 0.55 + sin(steps / 22 + ea_market_index(lhs)) * 0.18 - ea_market_index(rhs) * 0.015
    )
  })

  dependency_matrix <- expand.grid(x = focus_markets, y = focus_markets, stringsAsFactors = FALSE) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      value = purrr::map2_dbl(.data$x, .data$y, function(lhs, rhs) {
        if (identical(lhs, rhs)) {
          return(1)
        }

        0.72 - abs(ea_market_index(lhs) - ea_market_index(rhs)) * 0.22
      }),
      x_label = labels[.data$x],
      y_label = labels[.data$y]
    )

  primary <- focus_markets[[1]]
  relative_value <- tibble::tibble(
    date = dates,
    spread = cumsum(sin(seq_along(dates) / 15 + ea_market_index(primary)) * 0.09)
  )

  lead_lag <- tibble::tibble(
    horizon = c("1D", "3D", "1W", "2W"),
    score = c(0.18, 0.31, 0.24, 0.11),
    interpretation = c(
      "Products lead",
      "Cross-product transmission strongest",
      "Signal stabilizes",
      "Lagged decay"
    )
  )

  list(
    rolling_corr = rolling_corr,
    dependency_matrix = dependency_matrix,
    relative_value = relative_value,
    lead_lag = lead_lag,
    narrative = c(
      "Cross-product correlation is tighter at the front than the back.",
      "Intercommodity spreads are more informative than single-product level charts in this view."
    ),
    notes = c(
      "This page is intentionally framed as a cross-product workspace, not a single-product chart wall."
    ),
    assumptions = c(
      "Lead-lag values are placeholders.",
      "Matrix cells can later host correlation, beta, or dependency metrics."
    )
  )
}

ea_mock_seasonality_data <- function(filters) {
  catalog <- ea_market_catalog()
  markets <- ea_coalesce(filters$commodities, ea_global_filter_defaults(catalog)$commodities[1:2])
  labels <- stats::setNames(catalog$label, catalog$market)
  primary <- markets[[1]]

  seasonal_profile <- expand.grid(
    month = month.abb,
    market = markets,
    stringsAsFactors = FALSE
  ) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      label = labels[.data$market],
      value = sin(match(.data$month, month.abb) / 2 + ea_market_index(.data$market)) * 1.2
    )

  seasonal_heatmap <- expand.grid(
    year = 2020:2026,
    month = month.abb,
    stringsAsFactors = FALSE
  ) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      value = sin(match(.data$month, month.abb) / 1.7 + .data$year / 3) * 0.9
    )

  yoy_compare <- expand.grid(
    week = seq_len(52),
    year = 2023:2026,
    stringsAsFactors = FALSE
  ) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      value = sin(.data$week / 6 + .data$year / 5 + ea_market_index(primary)) * 0.8
    )

  seasonal_spread <- tibble::tibble(
    month = month.abb,
    spread = sin(seq_len(12) / 2.1 + ea_market_index(primary)) * 0.55
  )

  interpretation <- c(
    paste(ea_lookup_value(labels, primary), "shows the clearest seasonal inflection into summer structure."),
    "Seasonal analysis should eventually toggle between outright, calendar spread, and hedge views."
  )

  list(
    seasonal_profile = seasonal_profile,
    seasonal_heatmap = seasonal_heatmap,
    yoy_compare = yoy_compare,
    seasonal_spread = seasonal_spread,
    interpretation = interpretation,
    notes = c(
      "Seasonality modules support single-product and cross-product views from the same shell."
    ),
    assumptions = c(
      "Profiles are synthetic.",
      "Heatmaps are placeholders for calendar and week-based seasonality."
    )
  )
}

ea_mock_hedging_data <- function(filters) {
  catalog <- ea_market_catalog()
  labels <- stats::setNames(catalog$label, catalog$market)
  markets <- ea_coalesce(filters$commodities, ea_global_filter_defaults(catalog)$commodities[1:3])
  benchmark <- ea_coalesce(filters$comparison_commodity, ea_default_comparison(markets, catalog))
  tenors <- ea_curve_points(18)
  dates <- ea_dates(180)
  primary <- markets[[1]]

  kpis <- tibble::tribble(
    ~title, ~value, ~delta, ~icon, ~status,
    "Cross-hedge ratio", scales::number(0.74 + ea_market_index(primary) * 0.03, accuracy = 0.01), "front contract", "shield-alt", "positive",
    "Hedge stability", scales::percent(0.78, accuracy = 0.1), "63D lookback", "wave-square", "neutral",
    "Benchmark leg", ea_lookup_value(labels, benchmark), "cross-hedge anchor", "compass", "neutral",
    "Curve regime", "Steep front", "beta dispersion", "sliders-h", "warning"
  )

  hedge_term <- purrr::map_dfr(markets, function(market) {
    idx <- ea_market_index(market)
    tibble::tibble(
      tenor = tenors,
      market = market,
      label = ea_lookup_value(labels, market),
      ratio = 0.55 + idx * 0.05 + log1p(tenors) * 0.04 - tenors * 0.003
    )
  })

  hedge_time <- purrr::map_dfr(markets, function(market) {
    idx <- ea_market_index(market)
    tibble::tibble(
      date = dates,
      market = market,
      label = ea_lookup_value(labels, market),
      ratio = 0.58 + idx * 0.04 + sin(seq_along(dates) / 21 + idx) * 0.07
    )
  })

  hedge_matrix <- expand.grid(
    Product = labels[markets],
    Benchmark = labels[unique(c(benchmark, setdiff(markets, primary)))],
    stringsAsFactors = FALSE
  ) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      `Cross-Hedge Ratio` = scales::number(0.52 + dplyr::row_number() * 0.07, accuracy = 0.01),
      Stability = scales::percent(0.61 + dplyr::row_number() * 0.04, accuracy = 0.1),
      `Use Case` = c("Client cross-hedge", "Inventory hedge", "Proxy hedge", "Basis transfer")[((dplyr::row_number() - 1L) %% 4L) + 1L]
    )

  effectiveness <- tibble::tibble(
    bucket = c("Front", "Mid", "Back", "Cross"),
    effectiveness = c(0.72, 0.78, 0.66, 0.59)
  )

  sensitivity <- tibble::tibble(
    shock = c("-2 sigma", "-1 sigma", "Flat", "+1 sigma", "+2 sigma"),
    ratio = c(0.61, 0.68, 0.74, 0.81, 0.88)
  )

  list(
    kpis = kpis,
    hedge_term = hedge_term,
    hedge_time = hedge_time,
    hedge_matrix = hedge_matrix,
    effectiveness = effectiveness,
    sensitivity = sensitivity,
    notes = c(
      "Cross-Hedge is positioned as a market-making conversation surface rather than a portfolio-operations module.",
      "The KPI row, tenor chart, and history chart use a consistent visual contract for backend metrics."
    ),
    assumptions = c(
      "Ratios and effectiveness are UI placeholders.",
      "Benchmark selection currently follows the global comparison product."
    )
  )
}

ea_mock_scenario_data <- function(filters, shocks = list(flat = 0, vol = 0, spread = 0)) {
  catalog <- ea_market_catalog()
  labels <- stats::setNames(catalog$label, catalog$market)
  markets <- ea_coalesce(filters$commodities, ea_global_filter_defaults(catalog)$commodities[1:2])
  primary <- markets[[1]]
  tenors <- ea_curve_points(18)

  presets <- tibble::tribble(
    ~id, ~title, ~description, ~flat, ~vol, ~spread,
    "supply_squeeze", "Supply squeeze", "Front-end bullish shock with vol expansion.", 1.50, 6, 1.10,
    "refined_dislocation", "Refined dislocation", "Products widen while crude anchors the complex.", 0.80, 4, 2.20,
    "gas_volatility", "Gas vol transfer", "Gas-led stress that propagates into cross-hedge ratios.", 1.10, 8, 0.60
  )

  impact_curve <- tibble::tibble(
    tenor = tenors,
    base = sin(tenors / 3 + ea_market_index(primary)) * 0.2,
    impact = sin(tenors / 3 + ea_market_index(primary)) * 0.2 +
      shocks$flat * exp(-tenors / 14) +
      shocks$spread * 0.08
  )

  propagation <- tibble::tibble(
    factor = c("Outright", "Curve shape", "Intercommodity spread", "Volatility premium"),
    contribution = c(
      shocks$flat * 0.46,
      shocks$flat * 0.21,
      shocks$spread * 0.38,
      shocks$vol * 0.18
    )
  )

  kpis <- tibble::tribble(
    ~title, ~value, ~delta, ~icon, ~status,
    "Impact P&L", paste0(scales::dollar(shocks$flat * 2.8e5, largest_with_cents = 1e3)), "placeholder sensitivity", "dollar-sign", "neutral",
    "Curve shock", paste0(scales::number(shocks$spread * 0.42, accuracy = 0.01), " rel"), "front-back response", "chart-line", "warning",
    "Vol shift", paste0(scales::number(shocks$vol * 0.31, accuracy = 0.01), " pts"), "surface repricing", "wave-square", "warning",
    "Primary product", ea_lookup_value(labels, primary), "selected scenario scope", "compass", "neutral"
  )

  list(
    presets = presets,
    impact_curve = impact_curve,
    propagation = propagation,
    kpis = kpis,
    notes = c(
      "Scenario controls are intentionally compact so the page reads as an action surface.",
      "Preset cards can later call backend scenario engines without changing the UI contract."
    ),
    assumptions = c(
      "Impact figures are synthetic.",
      "Shocks currently drive only UI placeholders."
    )
  )
}

ea_mock_fundamentals_data <- function(filters) {
  catalog <- ea_market_catalog()
  markets <- ea_coalesce(filters$commodities, ea_global_filter_defaults(catalog)$commodities[1:2])
  labels <- stats::setNames(catalog$label, catalog$market)
  primary <- markets[[1]]
  primary_label <- ea_lookup_value(labels, primary)
  is_gas <- identical(primary, "NG")

  dates <- seq.Date(ea_reference_end_date() - 182, ea_reference_end_date(), by = "week")
  future_months <- format(seq.Date(as.Date(ea_reference_end_date()), by = "month", length.out = 6), "%b %Y")

  base_inventory <- ea_lookup_value(
    c(CL = 435, BRN = 355, RB = 228, HO = 162, NG = 2380),
    primary
  )
  seasonal_span <- ea_lookup_value(
    c(CL = 34, BRN = 28, RB = 18, HO = 16, NG = 280),
    primary
  )

  steps <- seq_along(dates)
  avg_5y <- base_inventory + sin(steps / 5 + ea_market_index(primary)) * seasonal_span * 0.18
  current <- avg_5y + cos(steps / 4.2 + ea_market_index(primary) / 2) * seasonal_span * 0.24

  inventory_curve <- tibble::tibble(
    date = dates,
    avg_5y = avg_5y,
    low_5y = avg_5y - seasonal_span * 0.42,
    high_5y = avg_5y + seasonal_span * 0.42,
    current = current
  )

  regional_locations <- if (is_gas) {
    c("East", "Midwest", "South Central", "Mountain", "Pacific")
  } else {
    c("Cushing", "USGC", "Midwest", "East Coast", "ARA")
  }

  regional_storage <- tibble::tibble(
    location = regional_locations,
    level = base_inventory / length(regional_locations) + seq_along(regional_locations) * seasonal_span * 0.18,
    delta_vs_5y = c(6.2, -3.1, 4.8, -1.7, 2.3) * if (is_gas) 4 else 1
  )

  balance_monitor <- tibble::tibble(
    month = future_months,
    balance = c(0.42, 0.28, -0.16, -0.34, -0.08, 0.19) * if (is_gas) 2.4 else 1,
    utilization = c(89.6, 90.8, 91.4, 90.1, 88.7, 87.9)
  )

  headlines <- tibble::tribble(
    ~time, ~source, ~tag, ~tone, ~headline, ~summary,
    "07:05", "EIA", "Inventory", "neutral", paste(primary_label, "stocks printed near the market median ahead of the open."), "Front structure remains more reactive than outright level after the release.",
    "08:20", "NOAA", "Weather", "warning", "Updated temperature outlook tightened the near-term gas demand window.", "Weather sensitivity remains concentrated in the prompt period and nearby basis hubs.",
    "09:10", "Pipeline", "Flows", "accent", "Maintenance guidance points to a short-lived flow constraint across a major corridor.", "Basis and storage optionality are likely to matter more than flat price if the outage extends.",
    "10:00", "Macro", "Policy", "neutral", "Refined product demand guidance was revised modestly higher into the next reporting window.", "Watch cracks and runs together rather than reading the update as a flat-price signal."
  )

  release_calendar <- tibble::tribble(
    ~Release, ~Next, ~Focus, ~Priority, ~Comment,
    "EIA Weekly Petroleum Status", "Wed 08:30 ET", "Crude / products", "High", "Headline inventory and runs release.",
    "API Weekly Stats", "Tue 16:30 ET", "Crude / products", "Medium", "Early inventory signal before EIA confirmation.",
    "EIA Natural Gas Storage", "Thu 10:30 ET", "Henry Hub NG", "High", "Storage delta and regional composition.",
    "NOAA 6-10 Day Outlook", "Daily 15:00 ET", "Weather", "Medium", "Demand risk for gas and power-linked products."
  )

  physical_signals <- tibble::tribble(
    ~Indicator, ~Latest, ~`vs 5Y`, ~Bias, ~Comment,
    "Commercial inventories", paste0(scales::number(current[[length(current)]], accuracy = 0.1), if (is_gas) " bcf" else " mbbl"), paste0(scales::number(current[[length(current)]] - avg_5y[[length(avg_5y)]], accuracy = 0.1), if (is_gas) " bcf" else " mbbl"), "Neutral/Tight", "Current stocks are holding inside the five-year band but trending tighter in the prompt window.",
    "Prompt basis", if (is_gas) "+0.18 $/MMBtu" else "+0.42 $/bbl", "+0.11", "Firm", "Basis is doing more of the work than flat price in the nearby structure.",
    "System utilization", "90.8%", "+1.6 pts", "Supportive", "Operational intensity is holding above seasonal norms.",
    "Export pull", if (is_gas) "14.2 bcf/d" else "4.1 mb/d", "+0.4", "Constructive", "Outbound pull is absorbing inventories faster than domestic demand alone."
  )

  inventory_axis <- if (is_gas) "Storage (bcf)" else "Inventories (mbbl)"
  regional_axis <- if (is_gas) "vs 5Y (bcf)" else "vs 5Y (mbbl)"
  balance_axis <- if (is_gas) "Implied balance (bcf/d)" else "Implied balance (mb/d)"

  kpis <- tibble::tribble(
    ~title, ~value, ~delta, ~icon, ~status,
    "Storage vs 5Y", paste0(scales::number(current[[length(current)]] - avg_5y[[length(avg_5y)]], accuracy = 0.1), if (is_gas) " bcf" else " mbbl"), "latest versus seasonal average", "warehouse", "neutral",
    "Weekly change", paste0(scales::number(current[[length(current)]] - current[[length(current) - 1]], accuracy = 0.1), if (is_gas) " bcf" else " mbbl"), "latest print", "arrow-trend-up", "accent",
    "System utilization", "90.8%", "operational intensity", "industry", "positive",
    "Prompt basis", if (is_gas) "+0.18 $/MMBtu" else "+0.42 $/bbl", "front location spread", "arrows-left-right", "warning",
    "Next release", "EIA", "Wed 08:30 ET", "calendar", "neutral"
  )

  list(
    kpis = kpis,
    inventory_curve = inventory_curve,
    regional_storage = regional_storage,
    balance_monitor = balance_monitor,
    headlines = headlines,
    release_calendar = release_calendar,
    physical_signals = physical_signals,
    inventory_title = if (is_gas) "Storage vs Five-Year Range" else "Inventories vs Five-Year Range",
    inventory_subtitle = paste(primary_label, "current trajectory versus the seasonal range."),
    inventory_axis = inventory_axis,
    regional_axis = regional_axis,
    balance_axis = balance_axis,
    notes = c(
      "Fundamentals is designed as a physical market context page for inventory, storage, and catalyst monitoring.",
      "Charts and tables are placeholders and keep a clean contract for backend attachment."
    ),
    assumptions = c(
      "Headline items are synthetic and should be replaced by a real news feed.",
      "Storage, balance, and utilization values are deterministic placeholders."
    )
  )
}
