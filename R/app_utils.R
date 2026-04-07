ea_market_catalog <- function() {
  required_markets <- tibble::tribble(
    ~market, ~label, ~complex, ~default_focus_order,
    "CL", "CL", "CRUDE", 1L,
    "NG", "NG", "GAS", 2L,
    "HTT", "HTT", "GAS", 3L,
    "BRN", "BRN", "CRUDE", 4L,
    "HO", "HO", "PRODUCTS", 5L,
    "RB", "RB", "PRODUCTS", 6L
  )

  registry <- ea_load_dataset("market_registry")

  missing_markets <- setdiff(required_markets$market, registry$market)
  if (length(missing_markets) > 0L) {
    cli::cli_abort("Market registry is missing required markets: {.val {missing_markets}}.")
  }

  if (!"label" %in% names(registry)) registry$label <- registry$market
  if (!"complex" %in% names(registry)) registry$complex <- "CROSS-COMMODITY"
  if (!"default_focus_order" %in% names(registry)) registry$default_focus_order <- seq_len(nrow(registry))

  registry %>%
    dplyr::filter(.data$market %in% required_markets$market) %>%
    dplyr::mutate(
      label = required_markets$label[match(.data$market, required_markets$market)],
      complex = dplyr::coalesce(required_markets$complex[match(.data$market, required_markets$market)], .data$complex),
      focus_order = dplyr::coalesce(required_markets$default_focus_order[match(.data$market, required_markets$market)], .data$default_focus_order, dplyr::row_number())
    ) %>%
    dplyr::arrange(.data$focus_order) %>%
    dplyr::select(dplyr::all_of(c("market", "label", "complex")))
}

ea_dashboard_timestamp <- function() {
  manifest <- ea_load_manifest()
  built_at <- manifest$built_at_utc

  if (is.null(built_at) || !nzchar(built_at)) {
    cli::cli_abort("Snapshot manifest is missing {.val built_at_utc}.")
  }

  lubridate::with_tz(
    as.POSIXct(built_at, tz = "UTC"),
    tzone = ea_coalesce(Sys.timezone(), "UTC")
  )
}

ea_reference_end_date <- function() {
  manifest <- ea_load_manifest()
  max_date <- manifest$datasets$commodity_curve_long$max_date
  if (is.null(max_date) || !nzchar(max_date)) {
    cli::cli_abort("Snapshot manifest is missing {.val commodity_curve_long.max_date}.")
  }
  as.Date(max_date)
}

ea_available_date_bounds <- function() {
  manifest <- ea_load_manifest()
  min_date <- manifest$datasets$commodity_curve_long$min_date
  max_date <- manifest$datasets$commodity_curve_long$max_date

  if (is.null(min_date) || !nzchar(min_date) || is.null(max_date) || !nzchar(max_date)) {
    cli::cli_abort("Snapshot manifest is missing commodity curve date bounds.")
  }

  c(as.Date(min_date), as.Date(max_date))
}

ea_dataset_has_rows <- function(dataset, required_cols = NULL) {
  obj <- tryCatch(
    ea_load_dataset(dataset),
    error = function(...) NULL
  )

  if (!is.data.frame(obj) || nrow(obj) == 0L) {
    return(FALSE)
  }

  if (!is.null(required_cols) && !all(required_cols %in% names(obj))) {
    return(FALSE)
  }

  TRUE
}

ea_page_availability <- function() {
  curves_ready <- ea_dataset_has_rows(
    "commodity_curve_long",
    required_cols = c("date", "market", "curve_point_num", "value")
  )
  options_ready <- ea_dataset_has_rows(
    "options_surface_long",
    required_cols = c("date", "market", "curve_point_num", "underlying_price")
  )

  tibble::tribble(
    ~value, ~enabled,
    "forward_curves", curves_ready,
    "volatility", curves_ready,
    "options", options_ready,
    "codynamics", curves_ready,
    "seasonality", curves_ready,
    "hedging", curves_ready
  )
}

ea_global_filter_defaults <- function(catalog = ea_market_catalog()) {
  date_bounds <- ea_available_date_bounds()

  list(
    market_complex = "CROSS-COMMODITY",
    commodities = catalog$market,
    comparison_commodity = "NG",
    date_range = c(max(date_bounds[2] - 365, date_bounds[1]), date_bounds[2]),
    rolling_window = "63D",
    tenor_bucket = c("Front", "Quarterly"),
    expiry_range = c(1, 12),
    regime = c("All Regimes")
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

ea_dates <- function(n = 180L, end_date = ea_reference_end_date()) {
  seq.Date(as.Date(end_date) - (n - 1L), as.Date(end_date), by = "day")
}

ea_curve_points <- function(n = 24L) {
  seq_len(n)
}

app_sys <- function(...) {
  installed_path <- system.file(..., package = "EnergyAnalytics")
  if (nzchar(installed_path)) {
    return(installed_path)
  }

  file.path(ea_project_root(), "inst", ...)
}

add_app_resources <- function() {
  resource_name <- "ea-www"
  resource_dir <- app_sys("app/www")
  existing_paths <- shiny::resourcePaths()

  if (
    dir.exists(resource_dir) &&
      (!resource_name %in% names(existing_paths) ||
        !identical(unname(existing_paths[[resource_name]]), resource_dir))
  ) {
    shiny::addResourcePath(resource_name, resource_dir)
  }

  htmltools::tagList(
    shiny::tags$head(
      shiny::tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = file.path(resource_name, "energy-analytics.css")
      )
    )
  )
}
