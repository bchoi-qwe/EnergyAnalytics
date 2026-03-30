EA_SCHEMA_VERSION <- "0.1.0"

ea_latest_data_dir <- function() {
  dev_dir <- file.path(ea_project_root(), "inst", "extdata", "latest")

  if (dir.exists(dev_dir)) {
    return(normalizePath(dev_dir, winslash = "/", mustWork = TRUE))
  }

  pkg_dir <- system.file("extdata", "latest", package = "EnergyAnalytics")
  if (nzchar(pkg_dir) && dir.exists(pkg_dir)) {
    return(normalizePath(pkg_dir, winslash = "/", mustWork = TRUE))
  }

  cli::cli_abort("Could not locate {file.path('inst', 'extdata', 'latest')}.")
}

ea_data_path <- function(dataset) {
  dataset_file <- if (grepl("\\.feather$", dataset)) dataset else paste0(dataset, ".feather")
  file.path(ea_latest_data_dir(), dataset_file)
}

ea_load_dataset <- function(dataset) {
  arrow::read_feather(ea_data_path(dataset), as_data_frame = TRUE)
}

ea_load_manifest <- function() {
  jsonlite::read_json(
    file.path(ea_latest_data_dir(), "manifest.json"),
    simplifyVector = TRUE
  )
}

ea_build_latest_snapshot <- function(latest_dir = NULL, release_dir = NULL, snapshot_id = NULL) {
  latest_dir <- latest_dir %||% file.path(ea_project_root(), "inst", "extdata", "latest")
  snapshot_id <- snapshot_id %||% ea_snapshot_id()

  dir.create(latest_dir, recursive = TRUE, showWarnings = FALSE)
  unlink(
    list.files(latest_dir, all.files = TRUE, no.. = TRUE, full.names = TRUE),
    recursive = TRUE,
    force = TRUE
  )

  datasets <- list(
    market_registry = ea_build_market_registry(),
    commodity_curve_long = ea_build_commodity_curve_long(),
    commodity_curve_wide = ea_build_commodity_curve_wide(),
    contract_expiry_metadata = ea_build_contract_expiry_metadata(),
    ust_curve_long = ea_build_ust_curve_long(),
    eia_stocks_long = ea_build_eia_stocks_long(),
    eia_storage_capacity_long = ea_build_eia_storage_capacity_long()
  )

  purrr::iwalk(
    datasets,
    ~ arrow::write_feather(
      x = .x,
      sink = file.path(latest_dir, paste0(.y, ".feather")),
      compression = "zstd"
    )
  )

  checksums <- ea_build_checksums(latest_dir)
  readr::write_tsv(checksums, file.path(latest_dir, "checksums.tsv"))

  manifest <- ea_build_manifest(
    datasets = datasets,
    checksums = checksums,
    snapshot_id = snapshot_id
  )

  jsonlite::write_json(
    x = manifest,
    path = file.path(latest_dir, "manifest.json"),
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )

  ea_validate_snapshot_dir(latest_dir)

  if (!is.null(release_dir)) {
    ea_create_snapshot_bundle(snapshot_dir = latest_dir, bundle_dir = release_dir)
  }

  invisible(manifest)
}

ea_create_snapshot_bundle <- function(snapshot_dir = NULL, bundle_dir = NULL) {
  snapshot_dir <- snapshot_dir %||% ea_latest_data_dir()
  bundle_dir <- bundle_dir %||% file.path(ea_project_root(), "artifacts", "releases")
  dir.create(bundle_dir, recursive = TRUE, showWarnings = FALSE)

  manifest <- jsonlite::read_json(
    file.path(snapshot_dir, "manifest.json"),
    simplifyVector = TRUE
  )
  snapshot_id <- manifest$snapshot_id

  staging_root <- tempfile(pattern = "ea_snapshot_")
  dir.create(staging_root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(staging_root, recursive = TRUE, force = TRUE), add = TRUE)

  staged_snapshot_dir <- file.path(staging_root, snapshot_id)
  dir.create(staged_snapshot_dir, recursive = TRUE, showWarnings = FALSE)

  files_to_copy <- list.files(snapshot_dir, full.names = TRUE)
  file.copy(files_to_copy, staged_snapshot_dir, overwrite = TRUE, recursive = FALSE)

  bundle_path <- file.path(
    bundle_dir,
    paste0("energy-analytics-snapshot-", snapshot_id, ".tar.gz")
  )

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(staging_root)

  tar_bin <- Sys.which("tar")
  utils::tar(
    tarfile = bundle_path,
    files = snapshot_id,
    compression = "gzip",
    tar = if (nzchar(tar_bin)) tar_bin else "tar"
  )

  invisible(bundle_path)
}

ea_validate_snapshot_dir <- function(snapshot_dir = NULL) {
  snapshot_dir <- snapshot_dir %||% ea_latest_data_dir()
  dataset_specs <- ea_dataset_specs()

  feather_files <- file.path(snapshot_dir, paste0(names(dataset_specs), ".feather"))
  missing_files <- feather_files[!file.exists(feather_files)]
  if (length(missing_files) > 0) {
    cli::cli_abort(c(
      "Missing snapshot files:",
      stats::setNames(as.list(missing_files), rep("x", length(missing_files)))
    ))
  }

  manifest_path <- file.path(snapshot_dir, "manifest.json")
  checksum_path <- file.path(snapshot_dir, "checksums.tsv")
  if (!file.exists(manifest_path) || !file.exists(checksum_path)) {
    cli::cli_abort("Snapshot manifest or checksum file is missing.")
  }

  loaded <- purrr::map(
    stats::setNames(names(dataset_specs), names(dataset_specs)),
    ~ arrow::read_feather(
      file.path(snapshot_dir, paste0(.x, ".feather")),
      as_data_frame = TRUE
    )
  )

  purrr::iwalk(loaded, function(df, nm) {
    missing_cols <- setdiff(dataset_specs[[nm]], names(df))
    if (length(missing_cols) > 0) {
      cli::cli_abort(
        "{.file {paste0(nm, '.feather')}} is missing required columns: {.val {missing_cols}}."
      )
    }
  })

  ea_assert_unique_key(
    loaded$commodity_curve_long,
    c("date", "market", "curve_point"),
    "commodity_curve_long"
  )
  ea_assert_unique_key(
    loaded$ust_curve_long,
    c("date", "market", "curve_point"),
    "ust_curve_long"
  )

  registry_markets <- loaded$market_registry$market
  commodity_markets <- sort(unique(loaded$commodity_curve_long$market))
  if (!identical(commodity_markets, sort(setdiff(registry_markets, "UST")))) {
    cli::cli_abort("Commodity curve markets do not match the market registry.")
  }

  ust_nodes <- loaded$ust_curve_long |>
    dplyr::distinct(curve_point) |>
    dplyr::pull(curve_point) |>
    sort()
  if (length(ust_nodes) != 11L) {
    cli::cli_abort("UST curve data does not contain all 11 maturity nodes.")
  }

  purrr::walk(
    list(loaded$commodity_curve_long, loaded$ust_curve_long),
    function(df) {
      ordered <- df |>
        dplyr::distinct(market, curve_point, curve_point_num) |>
        dplyr::arrange(market, curve_point_num)
      if (any(duplicated(ordered[, c("market", "curve_point_num")]))) {
        cli::cli_abort("Curve point ordering is not stable within markets.")
      }
    }
  )

  if (nrow(loaded$eia_stocks_long) == 0L || nrow(loaded$eia_storage_capacity_long) == 0L) {
    cli::cli_abort("Overlay datasets must be non-empty.")
  }

  checksums <- readr::read_tsv(checksum_path, show_col_types = FALSE)
  current_hashes <- ea_build_checksums(snapshot_dir)
  merged_hashes <- dplyr::left_join(
    checksums,
    current_hashes,
    by = c("file"),
    suffix = c("_manifest", "_current")
  )
  if (!all(merged_hashes$sha256_manifest == merged_hashes$sha256_current)) {
    cli::cli_abort("Snapshot checksums do not match the current files on disk.")
  }

  invisible(TRUE)
}

ea_project_root <- function() {
  opt_root <- getOption("EnergyAnalytics.project_root")
  if (!is.null(opt_root) && dir.exists(opt_root)) {
    return(normalizePath(opt_root, winslash = "/", mustWork = TRUE))
  }

  if (file.exists("DESCRIPTION")) {
    return(normalizePath(".", winslash = "/", mustWork = TRUE))
  }

  pkg_root <- system.file(package = "EnergyAnalytics")
  if (nzchar(pkg_root)) {
    return(normalizePath(pkg_root, winslash = "/", mustWork = TRUE))
  }

  cli::cli_abort(
    "Could not locate the project root. Set {.code options(EnergyAnalytics.project_root = ...)} first."
  )
}

ea_snapshot_id <- function(time = Sys.time()) {
  format(lubridate::with_tz(time, tzone = "UTC"), "%Y%m%dT%H%M%SZ")
}

ea_build_market_registry <- function() {
  tibble::tribble(
    ~market, ~label, ~market_type, ~unit, ~raw_unit, ~curve_kind, ~default_focus_order, ~source, ~source_family, ~supported_modules,
    "CL", "WTI Crude", "commodity", "USD/bbl", "USD/bbl", "futures_term_structure", 1L, "RTL", "RTL::dflong", "forward_curve;volatility_dynamics;seasonality;co_dynamics;hedge_ratios;options_greeks;risk_analytics;fundamentals_macro",
    "BRN", "Brent Crude", "commodity", "USD/bbl", "USD/bbl", "futures_term_structure", 2L, "RTL", "RTL::dflong", "forward_curve;volatility_dynamics;seasonality;co_dynamics;hedge_ratios;options_greeks;risk_analytics;fundamentals_macro",
    "NG", "Henry Hub Natural Gas", "commodity", "USD/MMBtu", "USD/MMBtu", "futures_term_structure", 3L, "RTL", "RTL::dflong", "forward_curve;volatility_dynamics;seasonality;co_dynamics;hedge_ratios;risk_analytics;fundamentals_macro",
    "HO", "ULSD", "commodity", "USD/gal", "USD/gal", "futures_term_structure", 4L, "RTL", "RTL::dflong", "forward_curve;volatility_dynamics;seasonality;co_dynamics;hedge_ratios;risk_analytics;fundamentals_macro",
    "RB", "RBOB Gasoline", "commodity", "USD/gal", "USD/gal", "futures_term_structure", 5L, "RTL", "RTL::dflong", "forward_curve;volatility_dynamics;seasonality;co_dynamics;hedge_ratios;risk_analytics;fundamentals_macro",
    "UST", "U.S. Treasuries", "rates", "percent", "percent", "yield_curve", 6L, "FRED", "tidyquant::tq_get", "forward_curve;volatility_dynamics;co_dynamics;hedge_ratios;fundamentals_macro"
  )
}

ea_build_commodity_curve_long <- function() {
  utils::data("dflong", package = "RTL", envir = environment())

  registry <- ea_build_market_registry() |>
    dplyr::filter(market_type == "commodity") |>
    dplyr::select(market, unit, raw_unit, market_type)

  dflong |>
    dplyr::mutate(
      market = stringr::str_extract(series, "^[A-Z]+"),
      curve_point_num = readr::parse_number(series),
      curve_point = stringr::str_pad(as.character(curve_point_num), width = 2, pad = "0")
    ) |>
    dplyr::filter(market %in% registry$market) |>
    dplyr::inner_join(registry, by = "market") |>
    dplyr::transmute(
      date = as.Date(date),
      market,
      curve_point,
      curve_point_num = as.numeric(curve_point_num),
      value = as.numeric(value),
      unit,
      market_type,
      source = "RTL",
      source_family = "RTL::dflong",
      source_series = series,
      frequency = "business_daily",
      raw_unit
    ) |>
    dplyr::arrange(date, market, curve_point_num)
}

ea_build_commodity_curve_wide <- function() {
  ea_build_commodity_curve_long() |>
    dplyr::select(date, source_series, value) |>
    tidyr::pivot_wider(names_from = source_series, values_from = value) |>
    dplyr::arrange(date)
}

ea_build_contract_expiry_metadata <- function() {
  utils::data("expiry_table", package = "RTL", envir = environment())

  market_map <- tibble::tribble(
    ~tick_prefix, ~market,
    "CL", "CL",
    "BZ", "BRN",
    "LCO", "BRN",
    "NG", "NG",
    "HO", "HO",
    "RB", "RB"
  )

  expiry_table |>
    dplyr::inner_join(market_map, by = c("tick.prefix" = "tick_prefix")) |>
    dplyr::transmute(
      market,
      source_market = cmdty,
      source_series = tick.prefix,
      contract_year = as.integer(Year),
      contract_month = as.integer(Month),
      contract_month_letter = Month.Letter,
      first_notice = as.Date(First.Notice),
      first_delivery = as.Date(First.Delivery),
      last_trade = as.Date(Last.Trade),
      last_delivery = as.Date(Last.Delivery),
      source = "RTL",
      source_family = "RTL::expiry_table"
    ) |>
    dplyr::group_by(market, source_series, contract_year, contract_month) |>
    dplyr::arrange(
      dplyr::desc(last_trade),
      dplyr::desc(first_notice),
      .by_group = TRUE
    ) |>
    dplyr::slice(1L) |>
    dplyr::ungroup() |>
    dplyr::arrange(market, contract_year, contract_month, source_series)
}

ea_build_ust_curve_long <- function() {
  spec <- ea_fred_curve_spec()

  purrr::pmap_dfr(
    spec,
    function(source_series, curve_point, curve_point_num, unit, raw_unit) {
      ea_fetch_fred_series(source_series) |>
        dplyr::transmute(
          date,
          market = "UST",
          curve_point = curve_point,
          curve_point_num = curve_point_num,
          value,
          unit = unit,
          market_type = "rates",
          source = "FRED",
          source_family,
          source_series = source_series,
          frequency = "business_daily",
          raw_unit = raw_unit
        )
    }
  ) |>
    dplyr::arrange(date, curve_point_num)
}

ea_build_eia_stocks_long <- function() {
  eia_key <- ea_require_env_var("EIA_API_KEY")
  stock_spec <- ea_eia_stock_spec()

  RTL::eia2tidy_all(
    tickers = dplyr::select(stock_spec, ticker, name),
    key = eia_key,
    long = TRUE
  ) |>
    dplyr::left_join(
      dplyr::select(stock_spec, ticker, name, product, location, unit, raw_unit, frequency),
      by = c("series" = "name")
    ) |>
    dplyr::transmute(
      date = as.Date(date),
      source_series = ticker,
      source_label = series,
      product,
      location,
      value = as.numeric(value),
      unit,
      raw_unit,
      source = "EIA",
      source_family = paste0("RTL::eia2tidy_all@", as.character(utils::packageVersion("RTL"))),
      frequency
    ) |>
    dplyr::arrange(product, location, date)
}

ea_build_eia_storage_capacity_long <- function() {
  eia_key <- ea_require_env_var("EIA_API_KEY")
  capacity_spec <- ea_eia_storage_capacity_spec()

  RTL::eia2tidy_all(
    tickers = dplyr::select(capacity_spec, ticker, name),
    key = eia_key,
    long = TRUE
  ) |>
    dplyr::left_join(
      dplyr::select(capacity_spec, ticker, name, product, location, unit, raw_unit, frequency),
      by = c("series" = "name")
    ) |>
    dplyr::transmute(
      date = as.Date(date),
      source_series = ticker,
      source_label = series,
      product,
      location,
      value = as.numeric(value),
      unit,
      raw_unit,
      source = "EIA",
      source_family = paste0("RTL::eia2tidy_all@", as.character(utils::packageVersion("RTL"))),
      frequency
    ) |>
    dplyr::arrange(date, product, source_series)
}

ea_build_checksums <- function(snapshot_dir) {
  files <- list.files(snapshot_dir, pattern = "\\.feather$", full.names = TRUE)

  tibble::tibble(
    file = basename(files),
    bytes = unname(file.info(files)$size),
    sha256 = purrr::map_chr(files, ~ digest::digest(file = .x, algo = "sha256"))
  ) |>
    dplyr::arrange(file)
}

ea_build_manifest <- function(datasets, checksums, snapshot_id) {
  build_time <- format(lubridate::with_tz(Sys.time(), "UTC"), "%Y-%m-%dT%H:%M:%SZ")

  dataset_entries <- purrr::imap(
    datasets,
    function(df, nm) {
      date_range <- ea_date_range(df)
      checksum_row <- checksums |>
        dplyr::filter(file == paste0(nm, ".feather"))

      list(
        file = paste0(nm, ".feather"),
        rows = nrow(df),
        columns = ncol(df),
        min_date = date_range$min_date,
        max_date = date_range$max_date,
        sha256 = checksum_row$sha256[[1]],
        bytes = checksum_row$bytes[[1]]
      )
    }
  )

  list(
    snapshot_id = snapshot_id,
    schema_version = EA_SCHEMA_VERSION,
    built_at_utc = build_time,
    sources = list(
      list(name = "RTL", version = paste0("RTL::dflong@", as.character(utils::packageVersion("RTL")))),
      list(name = "FRED", version = paste0("tidyquant::tq_get@", as.character(utils::packageVersion("tidyquant")))),
      list(name = "EIA", version = paste0("RTL::eia2tidy_all@", as.character(utils::packageVersion("RTL"))))
    ),
    datasets = dataset_entries
  )
}

ea_dataset_specs <- function() {
  list(
    market_registry = c(
      "market", "label", "market_type", "unit", "default_focus_order",
      "source", "source_family", "supported_modules"
    ),
    commodity_curve_long = c(
      "date", "market", "curve_point", "curve_point_num", "value",
      "unit", "market_type", "source"
    ),
    commodity_curve_wide = c("date"),
    contract_expiry_metadata = c(
      "market", "source_series", "contract_year", "contract_month",
      "last_trade", "source", "source_family"
    ),
    ust_curve_long = c(
      "date", "market", "curve_point", "curve_point_num", "value",
      "unit", "market_type", "source"
    ),
    eia_stocks_long = c(
      "date", "source_series", "product", "location", "value",
      "unit", "source"
    ),
    eia_storage_capacity_long = c(
      "date", "source_series", "product", "location", "value",
      "unit", "source"
    )
  )
}

ea_fred_curve_spec <- function() {
  tibble::tribble(
    ~source_series, ~curve_point, ~curve_point_num, ~unit, ~raw_unit,
    "DGS1MO", "1MO", 1 / 12, "percent", "percent",
    "DGS3MO", "3MO", 3 / 12, "percent", "percent",
    "DGS6MO", "6MO", 6 / 12, "percent", "percent",
    "DGS1", "1Y", 1, "percent", "percent",
    "DGS2", "2Y", 2, "percent", "percent",
    "DGS3", "3Y", 3, "percent", "percent",
    "DGS5", "5Y", 5, "percent", "percent",
    "DGS7", "7Y", 7, "percent", "percent",
    "DGS10", "10Y", 10, "percent", "percent",
    "DGS20", "20Y", 20, "percent", "percent",
    "DGS30", "30Y", 30, "percent", "percent"
  )
}

ea_fetch_fred_series <- function(series_id) {
  fred_raw <- suppressWarnings(
    tryCatch(
      tidyquant::tq_get(
        x = series_id,
        get = "economic.data"
      ),
      error = function(e) {
        cli::cli_abort("tq_get failed for FRED series {.val {series_id}}: {conditionMessage(e)}")
      }
    )
  )

  if (!is.data.frame(fred_raw) || !all(c("date", "price") %in% names(fred_raw))) {
    cli::cli_abort(
      "tq_get did not return a valid FRED data frame for series {.val {series_id}}."
    )
  }

  fred_raw |>
    dplyr::transmute(
      date = as.Date(date),
      value = as.numeric(price),
      source_family = paste0("tidyquant::tq_get@", as.character(utils::packageVersion("tidyquant")))
    ) |>
    dplyr::filter(!is.na(value))
}

ea_date_range <- function(df) {
  if (!"date" %in% names(df)) {
    return(list(min_date = NA_character_, max_date = NA_character_))
  }

  valid_dates <- df$date[!is.na(df$date)]
  if (length(valid_dates) == 0L) {
    return(list(min_date = NA_character_, max_date = NA_character_))
  }

  list(
    min_date = as.character(min(valid_dates)),
    max_date = as.character(max(valid_dates))
  )
}

ea_assert_unique_key <- function(df, key, dataset_name) {
  dupes <- df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(key))) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
    dplyr::filter(n > 1L)

  if (nrow(dupes) > 0L) {
    cli::cli_abort("Dataset {.val {dataset_name}} has duplicate rows on key {.val {key}}.")
  }

  invisible(TRUE)
}

ea_eia_stock_spec <- function() {
  tibble::tribble(
    ~ticker, ~name, ~product, ~location, ~unit, ~raw_unit, ~frequency,
    "PET.W_EPC0_SAX_YCUOK_MBBL.W", "CrudeCushing", "crude", "Cushing", "thousand_barrels", "thousand_barrels", "weekly",
    "PET.WGTSTUS1.W", "Gasoline", "gasoline", "US", "thousand_barrels", "thousand_barrels", "weekly",
    "PET.WDISTUS1.W", "ULSD", "distillate", "US", "thousand_barrels", "thousand_barrels", "weekly",
    "NG.NW2_EPG0_SWO_R48_BCF.W", "NGLower48", "ng", "Lower48", "bcf", "bcf", "weekly"
  )
}

ea_eia_storage_capacity_spec <- function() {
  tibble::tribble(
    ~ticker, ~name, ~product, ~location, ~unit, ~raw_unit, ~frequency,
    "PET.8_NA_8SWC_NUS_MBBL.A", "CrudeCapacityUS", "crude", "US", "thousand_barrels", "thousand_barrels", "annual",
    "PET.8_NA_8SWC_R10_MBBL.A", "CrudeCapacityP1", "crude", "P1", "thousand_barrels", "thousand_barrels", "annual",
    "PET.8_NA_8SWC_R20_MBBL.A", "CrudeCapacityP2", "crude", "P2", "thousand_barrels", "thousand_barrels", "annual",
    "PET.8_NA_8SWC_R30_MBBL.A", "CrudeCapacityP3", "crude", "P3", "thousand_barrels", "thousand_barrels", "annual",
    "PET.8_NA_8SWC_R40_MBBL.A", "CrudeCapacityP4", "crude", "P4", "thousand_barrels", "thousand_barrels", "annual",
    "PET.8_NA_8SWC_R50_MBBL.A", "CrudeCapacityP5", "crude", "P5", "thousand_barrels", "thousand_barrels", "annual",
    "NG.NGM_EPG0_SACW0_R48_MMCF.M", "NGCapacityLower48", "ng", "Lower48", "mmcf", "mmcf", "monthly"
  )
}

ea_require_env_var <- function(name) {
  value <- Sys.getenv(name, unset = "")
  if (!nzchar(value)) {
    cli::cli_abort("Environment variable {.val {name}} must be set.")
  }

  value
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
