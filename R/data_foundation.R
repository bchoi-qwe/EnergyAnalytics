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

ea_dataset_cache_env <- local({
  new.env(parent = emptyenv())
})

ea_load_dataset <- function(dataset) {
  dataset_path <- ea_data_path(dataset)
  dataset_info <- file.info(dataset_path)
  cache_key <- paste(
    normalizePath(dataset_path, winslash = "/", mustWork = FALSE),
    dataset_info$size[[1]],
    as.numeric(dataset_info$mtime[[1]]),
    sep = "::"
  )

  if (exists(cache_key, envir = ea_dataset_cache_env, inherits = FALSE)) {
    return(get(cache_key, envir = ea_dataset_cache_env, inherits = FALSE))
  }

  value <- arrow::read_feather(dataset_path, as_data_frame = TRUE)
  assign(cache_key, value, envir = ea_dataset_cache_env)
  value
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

  staging_dir <- tempfile(pattern = "ea_snapshot_build_")
  dir.create(staging_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(staging_dir, recursive = TRUE, force = TRUE), add = TRUE)

  contract_expiry_metadata <- ea_build_contract_expiry_metadata()
  commodity_curve_long <- ea_build_commodity_curve_long()
  commodity_contract_prices_long <- ea_build_zema_contract_prices_long(
    expiry_meta = contract_expiry_metadata,
    reference = commodity_curve_long
  )

  datasets <- list(
    market_registry = ea_build_market_registry(),
    commodity_curve_long = commodity_curve_long,
    commodity_curve_wide = ea_build_commodity_curve_wide(curves = commodity_curve_long),
    commodity_contract_prices_long = commodity_contract_prices_long,
    contract_expiry_metadata = contract_expiry_metadata,
    ust_curve_long = ea_build_ust_curve_long(),
    eia_stocks_long = ea_build_eia_stocks_long(),
    eia_storage_capacity_long = ea_build_eia_storage_capacity_long()
  )

  # Add options surface cube using in-memory dependencies
  datasets$options_surface_long <- ea_build_options_surface_long(
    contract_prices = datasets$commodity_contract_prices_long,
    ust_curve = datasets$ust_curve_long
  )

  purrr::iwalk(
    datasets,
    ~ arrow::write_feather(
      x = .x,
      sink = file.path(staging_dir, paste0(.y, ".feather")),
      compression = "zstd"
    )
  )

  checksums <- ea_build_checksums(staging_dir)
  readr::write_tsv(checksums, file.path(staging_dir, "checksums.tsv"))

  manifest <- ea_build_manifest(
    datasets = datasets,
    checksums = checksums,
    snapshot_id = snapshot_id
  )

  jsonlite::write_json(
    x = manifest,
    path = file.path(staging_dir, "manifest.json"),
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )

  ea_validate_snapshot_dir(staging_dir)

  if (!is.null(release_dir)) {
    ea_create_snapshot_bundle(snapshot_dir = staging_dir, bundle_dir = release_dir)
  }

  dir.create(latest_dir, recursive = TRUE, showWarnings = FALSE)
  unlink(
    list.files(latest_dir, all.files = TRUE, no.. = TRUE, full.names = TRUE),
    recursive = TRUE,
    force = TRUE
  )
  file.copy(
    from = list.files(staging_dir, full.names = TRUE),
    to = latest_dir,
    overwrite = TRUE,
    recursive = FALSE
  )

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
    cli::cli_abort("Missing snapshot files: {.val {basename(missing_files)}}.")
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
  ea_assert_unique_key(
    loaded$commodity_contract_prices_long,
    c("date", "market", "source_series"),
    "commodity_contract_prices_long"
  )

  registry_markets <- loaded$market_registry$market
  commodity_markets <- sort(unique(loaded$commodity_curve_long$market))
  if (!identical(commodity_markets, sort(setdiff(registry_markets, "UST")))) {
    cli::cli_abort("Commodity curve markets do not match the market registry.")
  }

  ust_nodes <- loaded$ust_curve_long %>%
    dplyr::distinct(.data$curve_point) %>%
    dplyr::pull(.data$curve_point) %>%
    sort()
  if (nrow(loaded$ust_curve_long) > 0L && length(ust_nodes) != 11L) {
    cli::cli_abort("UST curve data does not contain all 11 maturity nodes.")
  }

  purrr::walk(
    list(loaded$commodity_curve_long, loaded$ust_curve_long),
    function(df) {
      ordered <- df %>%
        dplyr::distinct(.data$market, .data$curve_point, .data$curve_point_num) %>%
        dplyr::arrange(.data$market, .data$curve_point_num)
      if (any(duplicated(ordered[, c("market", "curve_point_num")]))) {
        cli::cli_abort("Curve point ordering is not stable within markets.")
      }
    }
  )

  required_non_empty <- c(
    "commodity_curve_long",
    "commodity_contract_prices_long",
    "ust_curve_long",
    "eia_stocks_long",
    "eia_storage_capacity_long",
    "options_surface_long"
  )

  purrr::walk(required_non_empty, function(nm) {
    if (nrow(loaded[[nm]]) == 0L) {
      cli::cli_abort("Dataset {.val {nm}} must be non-empty.")
    }
  })

  purrr::iwalk(loaded, function(df, nm) {
    if ("source_family" %in% names(df) && any(grepl(";", df$source_family, fixed = TRUE), na.rm = TRUE)) {
      cli::cli_abort("Dataset {.val {nm}} declares multiple source families in a single field.")
    }
  })

  if (!all(loaded$commodity_curve_long$source_family == "RTL::dflong")) {
    cli::cli_abort("{.val commodity_curve_long} must be sourced only from {.val RTL::dflong}.")
  }

  if (!all(loaded$commodity_contract_prices_long$source_family == "RTL::getPrices")) {
    cli::cli_abort("{.val commodity_contract_prices_long} must be sourced only from {.val RTL::getPrices}.")
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
    "HTT", "HTT", "commodity", "USD/MMBtu", "USD/MMBtu", "futures_term_structure", 4L, "RTL", "RTL::dflong", "forward_curve;volatility_dynamics;seasonality;co_dynamics;hedge_ratios;risk_analytics;fundamentals_macro",
    "HO", "ULSD", "commodity", "USD/gal", "USD/gal", "futures_term_structure", 5L, "RTL", "RTL::dflong", "forward_curve;volatility_dynamics;seasonality;co_dynamics;hedge_ratios;risk_analytics;fundamentals_macro",
    "RB", "RBOB Gasoline", "commodity", "USD/gal", "USD/gal", "futures_term_structure", 6L, "RTL", "RTL::dflong", "forward_curve;volatility_dynamics;seasonality;co_dynamics;hedge_ratios;risk_analytics;fundamentals_macro",
    "UST", "U.S. Treasuries", "rates", "percent", "percent", "yield_curve", 7L, "FRED", "tidyquant::tq_get", "forward_curve;volatility_dynamics;co_dynamics;hedge_ratios;fundamentals_macro"
  )
}

ea_build_commodity_reference_long <- function() {
  utils::data("dflong", package = "RTL", envir = environment())

  registry <- ea_build_market_registry() %>%
    dplyr::filter(.data$market_type == "commodity") %>%
    dplyr::select(.data$market, .data$unit, .data$raw_unit, .data$market_type)

  dflong %>%
    dplyr::mutate(
      market = stringr::str_extract(.data$series, "^[A-Z]+"),
      curve_point_num = readr::parse_number(.data$series),
      curve_point = stringr::str_pad(as.character(.data$curve_point_num), width = 2, pad = "0")
    ) %>%
    dplyr::filter(.data$market %in% registry$market) %>%
    dplyr::inner_join(registry, by = "market") %>%
    dplyr::transmute(
      date = as.Date(.data$date),
      .data$market,
      .data$curve_point,
      curve_point_num = as.numeric(.data$curve_point_num),
      value = as.numeric(.data$value),
      .data$unit,
      .data$market_type,
      source = "RTL",
      source_family = "RTL::dflong",
      source_series = .data$series,
      frequency = "business_daily",
      .data$raw_unit
    ) %>%
    dplyr::arrange(.data$date, .data$market, .data$curve_point_num)
}

ea_zema_futures_spec <- function() {
  tibble::tribble(
    ~market, ~feed, ~symbol_root, ~symbol_needs_at, ~max_contracts,
    "CL", "CME_NymexFutures_EOD", "CL", FALSE, 36L,
    "BRN", "ICE_EuroFutures", "BRN", FALSE, 36L,
    "NG", "CME_NymexFutures_EOD", "NG", FALSE, 36L,
    "HO", "CME_NymexFutures_EOD", "HO", FALSE, 18L,
    "RB", "CME_NymexFutures_EOD", "RB", FALSE, 18L
  )
}

ea_rtl_credentials <- function(required = FALSE) {
  iuser <- ea_optional_env_var("RTL_IUSER")
  ipassword <- ea_optional_env_var("RTL_IPASSWORD")

  if (required && (is.null(iuser) || is.null(ipassword))) {
    cli::cli_abort("Environment variables {.val RTL_IUSER} and {.val RTL_IPASSWORD} must be set.")
  }

  list(iuser = iuser, ipassword = ipassword)
}

ea_has_rtl_credentials <- function() {
  creds <- ea_rtl_credentials(required = FALSE)
  !is.null(creds$iuser) && !is.null(creds$ipassword)
}

ea_validate_rtl_credentials <- local({
  cache <- NULL

  function(force = FALSE) {
    if (!ea_has_rtl_credentials()) {
      return(FALSE)
    }

    if (!force && isTRUE(cache)) {
      return(TRUE)
    }

    creds <- ea_rtl_credentials(required = TRUE)
    url <- httr::modify_url(
      url = "https://data.zema.global",
      path = "/lds/feeds/CME_NymexFutures_EOD/ts?Symbol=CL9Z&fromDateTime=2019-01-01"
    )

    res <- tryCatch(
      httr::GET(
        url = url,
        httr::authenticate(user = creds$iuser, password = creds$ipassword, type = "basic")
      ),
      error = function(e) e
    )

    if (inherits(res, "error")) {
      cli::cli_abort(
        "RTL/ZEMA preflight request failed before authentication completed: {conditionMessage(res)}"
      )
    }

    status <- httr::status_code(res)
    if (identical(status, 401L)) {
      cli::cli_abort(
        "RTL/ZEMA credentials were rejected by {.url https://data.zema.global} (HTTP 401). Confirm {.val RTL_IUSER}, {.val RTL_IPASSWORD}, and feed entitlement."
      )
    }

    if (status >= 400L) {
      cli::cli_abort(
        "RTL/ZEMA preflight request failed with HTTP status {.val {status}}."
      )
    }

    cache <<- TRUE
    TRUE
  }
})

ea_zema_empty_prices_long <- function() {
  tibble::tibble(
    date = as.Date(character()),
    feed = character(),
    contract_code = character(),
    value = numeric()
  )
}

ea_zema_fetch_price_direct <- function(feed, contract, from, iuser, ipassword) {
  url <- httr::modify_url(
    url = "https://data.zema.global",
    path = paste0(
      "/lds/feeds/",
      feed,
      "/ts?Symbol=",
      utils::URLencode(contract, reserved = TRUE),
      "&fromDateTime=",
      from
    )
  )

  attempt <- 1L
  max_attempts <- 5L
  repeat {
    res <- tryCatch(
      httr::GET(
        url = url,
        httr::authenticate(user = iuser, password = ipassword, type = "basic")
      ),
      error = function(e) e
    )

    if (inherits(res, "error")) {
      cli::cli_abort(
        "ZEMA fetch failed for feed {.val {feed}} contract {.val {sub('^@', '', contract)}}: {conditionMessage(res)}"
      )
    }

    status <- httr::status_code(res)
    if (!identical(status, 429L) || attempt >= max_attempts) {
      break
    }

    Sys.sleep(min(2^(attempt - 1L), 8))
    attempt <- attempt + 1L
  }

  if (status >= 400L) {
    cli::cli_abort(
      "ZEMA fetch failed for feed {.val {feed}} contract {.val {sub('^@', '', contract)}} with HTTP status {.val {status}}."
    )
  }

  payload_text <- httr::content(res, as = "text", encoding = "UTF-8")
  if (!nzchar(payload_text) || identical(trimws(payload_text), "{}")) {
    return(ea_zema_empty_prices_long())
  }

  payload <- jsonlite::fromJSON(payload_text, simplifyVector = FALSE)
  series <- payload$series[[1]] %||% NULL
  dates <- unlist(series$dates %||% character(), use.names = FALSE)
  values <- series$values %||% list()
  value_idx <- if (identical(feed, "CME_NymexOptions_EOD")) 4L else 1L

  if (length(dates) == 0L || length(values) < value_idx) {
    return(ea_zema_empty_prices_long())
  }

  items <- unlist(values[[value_idx]]$item %||% character(), use.names = FALSE)
  if (length(items) == 0L) {
    return(ea_zema_empty_prices_long())
  }

  tibble::tibble(
    date = as.Date(dates),
    feed = feed,
    contract_code = sub("^@", "", contract),
    value = as.numeric(items)
  ) %>%
    dplyr::filter(!is.na(.data$value))
}

ea_zema_fetch_prices_long <- function(feed, contracts, from, iuser, ipassword, batch_size = 25L, allow_partial = FALSE) {
  contracts <- unique(stats::na.omit(contracts))

  if (length(contracts) == 0L) {
    return(ea_zema_empty_prices_long())
  }

  if (allow_partial) {
    return(purrr::map_dfr(
      contracts,
      ~ ea_zema_fetch_price_direct(
        feed = feed,
        contract = .x,
        from = from,
        iuser = iuser,
        ipassword = ipassword
      )
    ))
  }

  batches <- split(contracts, ceiling(seq_along(contracts) / batch_size))

  purrr::map_dfr(batches, function(batch) {
    raw_batch <- tryCatch(
      RTL::getPrices(
        feed = feed,
        contracts = batch,
        from = from,
        iuser = iuser,
        ipassword = ipassword
      ),
      error = function(e) {
        cli::cli_abort(
          "ZEMA fetch failed for feed {.val {feed}} ({length(batch)} contracts): {conditionMessage(e)}"
        )
      }
    )

    if (!is.data.frame(raw_batch) || nrow(raw_batch) == 0L) {
        cli::cli_abort("ZEMA feed {.val {feed}} returned no rows for {.val {length(batch)}} requested contracts.")
      }

    tibble::as_tibble(raw_batch) %>%
      dplyr::mutate(date = as.Date(.data$date)) %>%
      tidyr::pivot_longer(
        cols = -dplyr::all_of("date"),
        names_to = "contract_code",
        values_to = "value"
      ) %>%
      dplyr::mutate(
        feed = feed,
        contract_code = sub("^@", "", .data$contract_code),
        value = as.numeric(.data$value)
      ) %>%
      dplyr::filter(!is.na(.data$value))
  })
}

ea_build_zema_contract_prices_long <- function(expiry_meta = NULL, reference = NULL, from = NULL) {
  ea_rtl_credentials(required = TRUE)
  ea_validate_rtl_credentials()

  expiry_meta <- expiry_meta %||% ea_build_contract_expiry_metadata()
  reference <- reference %||% ea_build_commodity_reference_long()
  from <- as.character(as.Date(from %||% (max(reference$date, na.rm = TRUE) - lubridate::years(3))))

  creds <- ea_rtl_credentials(required = TRUE)
  spec <- ea_zema_futures_spec()
  registry <- ea_build_market_registry() %>%
    dplyr::filter(.data$market %in% spec$market) %>%
    dplyr::select(.data$market, .data$unit, .data$raw_unit, .data$market_type)

  contract_meta <- expiry_meta %>%
    dplyr::filter(.data$market %in% spec$market, !is.na(.data$last_trade), .data$last_trade >= as.Date(from)) %>%
    dplyr::group_by(.data$market, .data$contract_year, .data$contract_month) %>%
    dplyr::arrange(dplyr::desc(.data$last_trade), .by_group = TRUE) %>%
    dplyr::summarise(
      contract_month_letter = dplyr::first(.data$contract_month_letter),
      last_trade = dplyr::first(.data$last_trade),
      .groups = "drop"
    ) %>%
    dplyr::inner_join(spec, by = "market") %>%
    dplyr::mutate(
      fetch_contract = paste0(
        dplyr::if_else(.data$feed == "CME_NymexFutures_EOD", "@", ""),
        .data$symbol_root,
        sprintf("%02d", .data$contract_year %% 100L),
        .data$contract_month_letter
      ),
      contract_code = sub("^@", "", .data$fetch_contract)
    )

  if (nrow(contract_meta) == 0L) {
    cli::cli_abort("No listed futures contract metadata was available to build {.val commodity_contract_prices_long}.")
  }

  quotes <- contract_meta %>%
    dplyr::distinct(.data$feed, .data$fetch_contract) %>%
    dplyr::group_by(.data$feed) %>%
    dplyr::group_split()

  quotes_long <- purrr::map_dfr(quotes, function(feed_group) {
    ea_zema_fetch_prices_long(
      feed = feed_group$feed[[1]],
      contracts = feed_group$fetch_contract,
      from = from,
      iuser = creds$iuser,
      ipassword = creds$ipassword
    )
  })

  if (nrow(quotes_long) == 0L) {
    cli::cli_abort("ZEMA returned no listed futures prices for {.val commodity_contract_prices_long}.")
  }

  quotes_long %>%
    dplyr::inner_join(
      contract_meta %>%
        dplyr::select(
          .data$market,
          .data$feed,
          .data$contract_code,
          .data$contract_year,
          .data$contract_month,
          .data$contract_month_letter,
          .data$last_trade
        ),
      by = c("feed", "contract_code")
    ) %>%
    dplyr::inner_join(registry, by = "market") %>%
    dplyr::transmute(
      date = as.Date(.data$date),
      .data$market,
      .data$contract_code,
      contract_year = as.integer(.data$contract_year),
      contract_month = as.integer(.data$contract_month),
      .data$contract_month_letter,
      last_trade = as.Date(.data$last_trade),
      value = as.numeric(.data$value),
      .data$unit,
      .data$market_type,
      source = "RTL",
      source_family = "RTL::getPrices",
      source_series = .data$contract_code,
      source_feed = .data$feed,
      frequency = "business_daily",
      .data$raw_unit
    ) %>%
    dplyr::arrange(.data$date, .data$market, .data$last_trade)
}

ea_build_commodity_curve_long <- function(reference = NULL) {
  reference <- reference %||% ea_build_commodity_reference_long()

  reference %>%
    dplyr::arrange(.data$date, .data$market, .data$curve_point_num)
}

ea_build_commodity_curve_wide <- function(curves = NULL) {
  curves <- curves %||% ea_build_commodity_curve_long()

  curves %>%
    dplyr::select(.data$date, .data$source_series, .data$value) %>%
    tidyr::pivot_wider(names_from = .data$source_series, values_from = .data$value) %>%
    dplyr::arrange(.data$date)
}

ea_build_contract_expiry_metadata <- function() {
  utils::data("expiry_table", package = "RTL", envir = environment())

  market_map <- tibble::tribble(
    ~tick_prefix, ~market,
    "CL", "CL",
    "BZ", "BRN",
    "LCO", "BRN",
    "NG", "NG",
    "HTT", "HTT",
    "HO", "HO",
    "RB", "RB"
  )

  expiry_table %>%
    dplyr::inner_join(market_map, by = c("tick.prefix" = "tick_prefix")) %>%
    dplyr::transmute(
      .data$market,
      source_market = .data$cmdty,
      source_series = .data$tick.prefix,
      contract_year = as.integer(.data$Year),
      contract_month = as.integer(.data$Month),
      contract_month_letter = .data$Month.Letter,
      first_notice = as.Date(.data$First.Notice),
      first_delivery = as.Date(.data$First.Delivery),
      last_trade = as.Date(.data$Last.Trade),
      last_delivery = as.Date(.data$Last.Delivery),
      source = "RTL",
      source_family = "RTL::expiry_table"
    ) %>%
    dplyr::group_by(.data$market, .data$source_series, .data$contract_year, .data$contract_month) %>%
    dplyr::arrange(
      dplyr::desc(.data$last_trade),
      dplyr::desc(.data$first_notice),
      .by_group = TRUE
    ) %>%
    dplyr::slice(1L) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data$market, .data$contract_year, .data$contract_month, .data$source_series)
}

ea_build_ust_curve_long <- function() {
  spec <- ea_fred_curve_spec()

  purrr::pmap_dfr(
    spec,
    function(source_series, curve_point, curve_point_num, unit, raw_unit) {
      ea_fetch_fred_series(source_series) %>%
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
  ) %>%
    dplyr::arrange(date, curve_point_num)
}

ea_build_eia_stocks_long <- function() {
  eia_key <- ea_require_env_var("EIA_API_KEY")
  stock_spec <- ea_eia_stock_spec()

  RTL::eia2tidy_all(
    tickers = dplyr::select(stock_spec, ticker, name),
    key = eia_key,
    long = TRUE
  ) %>%
    dplyr::left_join(
      dplyr::select(stock_spec, ticker, name, product, location, unit, raw_unit, frequency),
      by = c("series" = "name")
    ) %>%
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
    ) %>%
    dplyr::arrange(product, location, date)
}

ea_build_eia_storage_capacity_long <- function() {
  eia_key <- ea_require_env_var("EIA_API_KEY")
  capacity_spec <- ea_eia_storage_capacity_spec()

  RTL::eia2tidy_all(
    tickers = dplyr::select(capacity_spec, ticker, name),
    key = eia_key,
    long = TRUE
  ) %>%
    dplyr::left_join(
      dplyr::select(capacity_spec, ticker, name, product, location, unit, raw_unit, frequency),
      by = c("series" = "name")
    ) %>%
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
    ) %>%
    dplyr::arrange(date, product, source_series)
}

ea_build_checksums <- function(snapshot_dir) {
  files <- list.files(snapshot_dir, pattern = "\\.feather$", full.names = TRUE)

  tibble::tibble(
    file = basename(files),
    bytes = unname(file.info(files)$size),
    sha256 = purrr::map_chr(files, ~ digest::digest(file = .x, algo = "sha256"))
  ) %>%
    dplyr::arrange(file)
}

ea_build_manifest <- function(datasets, checksums, snapshot_id) {
  build_time <- format(lubridate::with_tz(Sys.time(), "UTC"), "%Y-%m-%dT%H:%M:%SZ")

  dataset_entries <- purrr::imap(
    datasets,
    function(df, nm) {
      date_range <- ea_date_range(df)
      checksum_row <- checksums %>%
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

  sources <- list(
    list(name = "RTL", version = paste0("RTL::dflong@", as.character(utils::packageVersion("RTL")))),
    list(name = "ZEMA", version = paste0("RTL::getPrices@", as.character(utils::packageVersion("RTL")))),
    list(name = "FRED", version = paste0("tidyquant::tq_get@", as.character(utils::packageVersion("tidyquant")))),
    list(name = "EIA", version = paste0("RTL::eia2tidy_all@", as.character(utils::packageVersion("RTL"))))
  )

  list(
    snapshot_id = snapshot_id,
    schema_version = EA_SCHEMA_VERSION,
    built_at_utc = build_time,
    sources = sources,
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
    commodity_contract_prices_long = c(
      "date", "market", "contract_code", "contract_year", "contract_month",
      "last_trade", "value", "source", "source_family", "source_series", "source_feed"
    ),
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
    ),
    options_surface_long = c(
      "date", "market", "curve_point_num", "contract_month", "days_to_expiry", "strike",
      "option_type", "underlying_price", "implied_volatility",
      "delta", "gamma", "vega", "theta", "rho", "vanna", "charm",
      "speed", "zomma", "color", "ultima",
      "source", "source_family", "source_feed", "source_series"
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
    cli::cli_abort("tq_get did not return a valid FRED data frame for series {.val {series_id}}.")
  }

  fred_raw %>%
    dplyr::transmute(
      date = as.Date(date),
      value = as.numeric(price),
      source_family = paste0("tidyquant::tq_get@", as.character(utils::packageVersion("tidyquant")))
    ) %>%
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
  dupes <- df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(key))) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
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
  if (!nzchar(value) || ea_env_var_is_placeholder(value)) {
    cli::cli_abort("Environment variable {.val {name}} must be set.")
  }

  value
}

ea_optional_env_var <- function(name) {
  value <- Sys.getenv(name, unset = "")
  if (!nzchar(value) || ea_env_var_is_placeholder(value)) {
    return(NULL)
  }

  value
}

ea_env_var_is_placeholder <- function(value) {
  if (!nzchar(value)) {
    return(TRUE)
  }

  placeholder_patterns <- c(
    "^your_.*_here$",
    "^example$",
    "^example_.*$",
    "^replace_me$",
    "^changeme$",
    "^<.*>$"
  )

  any(vapply(
    placeholder_patterns,
    function(pattern) grepl(pattern, value, perl = TRUE, ignore.case = TRUE),
    logical(1)
  ))
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
