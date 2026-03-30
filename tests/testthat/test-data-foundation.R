if (!exists("ea_project_root", mode = "function")) {
  candidate_roots <- c(".", "..", "../..")
  source_root <- candidate_roots[file.exists(file.path(candidate_roots, "R", "data_foundation.R"))][1]

  if (is.na(source_root)) {
    stop("Could not locate R/data_foundation.R for direct test execution.", call. = FALSE)
  }

  options(EnergyAnalytics.project_root = normalizePath(source_root, winslash = "/", mustWork = TRUE))
  source(file.path(source_root, "R", "data_foundation.R"), local = FALSE)
}

skip_if_no_snapshot <- function() {
  snapshot_dir <- tryCatch(
    ea_latest_data_dir(),
    error = function(...) NA_character_
  )

  if (!is.character(snapshot_dir) || !nzchar(snapshot_dir) || !file.exists(file.path(snapshot_dir, "manifest.json"))) {
    testthat::skip("No local snapshot present. Run scripts/build-data.R after setting API keys.")
  }
}

ea_policy_audit_text <- function() {
  source_root <- ea_project_root()
  audit_files <- c(
    list.files(file.path(source_root, "R"), pattern = "\\.[Rr]$", full.names = TRUE),
    list.files(file.path(source_root, "data-raw"), pattern = "\\.[Rr]$", full.names = TRUE)
  )

  if (length(audit_files) > 0L) {
    return(paste(vapply(audit_files, readr::read_file, character(1)), collapse = "\n"))
  }

  audit_functions <- c(
    "ea_build_market_registry",
    "ea_build_commodity_curve_long",
    "ea_build_contract_expiry_metadata",
    "ea_build_eia_stocks_long",
    "ea_build_eia_storage_capacity_long",
    "ea_fetch_fred_series"
  )

  paste(vapply(
    audit_functions,
    function(name) {
      fn <- get(name, envir = asNamespace("EnergyAnalytics"))
      paste(deparse(body(fn), width.cutoff = 500L), collapse = "\n")
    },
    character(1)
  ), collapse = "\n")
}

test_that("all canonical feather datasets load", {
  skip_if_no_snapshot()
  snapshot_dir <- ea_latest_data_dir()
  files <- list.files(snapshot_dir, pattern = "\\.feather$", full.names = FALSE)

  expect_setequal(
    files,
    c(
      "market_registry.feather",
      "commodity_curve_long.feather",
      "commodity_curve_wide.feather",
      "contract_expiry_metadata.feather",
      "ust_curve_long.feather",
      "eia_stocks_long.feather",
      "eia_storage_capacity_long.feather"
    )
  )

  invisible(lapply(files, function(file) {
    df <- arrow::read_feather(file.path(snapshot_dir, file), as_data_frame = TRUE)
    expect_s3_class(df, "data.frame")
    expect_gt(nrow(df), 0)
  }))
})

test_that("curve datasets satisfy canonical schema constraints", {
  skip_if_no_snapshot()
  commodity <- ea_load_dataset("commodity_curve_long")
  expiry <- ea_load_dataset("contract_expiry_metadata")
  ust <- ea_load_dataset("ust_curve_long")
  registry <- ea_load_dataset("market_registry")

  expect_equal(anyDuplicated(commodity[, c("date", "market", "curve_point")]), 0L)
  expect_equal(anyDuplicated(ust[, c("date", "market", "curve_point")]), 0L)
  expect_equal(anyDuplicated(expiry[, c("market", "source_series", "contract_year", "contract_month")]), 0L)

  expect_setequal(sort(unique(commodity$market)), sort(setdiff(registry$market, "UST")))
  expect_equal(sort(unique(ust$market)), "UST")
  expect_equal(length(unique(ust$curve_point)), 11L)
  expect_setequal(sort(unique(expiry$market)), c("BRN", "CL", "HO", "NG", "RB"))
})

test_that("overlay datasets are non-empty and dated", {
  skip_if_no_snapshot()
  stocks <- ea_load_dataset("eia_stocks_long")
  capacity <- ea_load_dataset("eia_storage_capacity_long")

  expect_gt(nrow(stocks), 0)
  expect_gt(nrow(capacity), 0)
  expect_true("date" %in% names(stocks))
  expect_true("date" %in% names(capacity))
  expect_false(all(is.na(stocks$date)))
  expect_false(all(is.na(capacity$date)))
  expect_true(all(stocks$source == "EIA"))
  expect_true(all(capacity$source == "EIA"))
})

test_that("manifest hashes match current files", {
  skip_if_no_snapshot()
  snapshot_dir <- ea_latest_data_dir()
  manifest <- ea_load_manifest()
  checksum_file <- readr::read_tsv(
    file.path(snapshot_dir, "checksums.tsv"),
    show_col_types = FALSE
  )

  expect_equal(manifest$schema_version, "0.1.0")
  expect_true(length(manifest$datasets) == 7L)
  expect_setequal(manifest$sources$name, c("RTL", "FRED", "EIA"))

  current <- lapply(checksum_file$file, function(file) {
    digest::digest(file = file.path(snapshot_dir, file), algo = "sha256")
  })
  current <- unlist(current, use.names = FALSE)

  expect_equal(checksum_file$sha256, current)
})

test_that("snapshot bundle reproduces latest files", {
  skip_if_no_snapshot()
  bundle_dir <- file.path(tempdir(), "bundle-out")
  dir.create(bundle_dir, recursive = TRUE, showWarnings = FALSE)

  bundle_path <- ea_create_snapshot_bundle(bundle_dir = bundle_dir)
  expect_true(file.exists(bundle_path))

  extract_dir <- file.path(tempdir(), "bundle-extract")
  unlink(extract_dir, recursive = TRUE, force = TRUE)
  dir.create(extract_dir, recursive = TRUE, showWarnings = FALSE)

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(extract_dir)
  utils::untar(bundle_path)

  manifest <- ea_load_manifest()
  extracted_snapshot_dir <- file.path(extract_dir, manifest$snapshot_id)
  extracted_checksums <- readr::read_tsv(
    file.path(extracted_snapshot_dir, "checksums.tsv"),
    show_col_types = FALSE
  )

  current_checksums <- readr::read_tsv(
    file.path(ea_latest_data_dir(), "checksums.tsv"),
    show_col_types = FALSE
  )

  expect_equal(extracted_checksums, current_checksums)
})

test_that("source policy excludes forbidden bundled RTL datasets", {
  audit_text <- ea_policy_audit_text()
  forbidden_terms <- c(
    "RTL::eiaStocks",
    "RTL::eiaStorageCap",
    "RTL::futuresRef",
    "data(\"eiaStocks\"",
    "data(\"eiaStorageCap\"",
    "data(\"futuresRef\"",
    "dfwide",
    "readxl::read_excel",
    "download.file(",
    "fredr::fredr",
    "fredr::fredr_set_key",
    "FRED_API_KEY"
  )

  expect_false(any(vapply(
    forbidden_terms,
    function(term) grepl(term, audit_text, fixed = TRUE),
    logical(1)
  )))
})

test_that("source policy requires the intended live source clients", {
  audit_text <- ea_policy_audit_text()

  expect_true(grepl("RTL::dflong", audit_text, fixed = TRUE))
  expect_true(grepl("RTL::eia2tidy_all", audit_text, fixed = TRUE))
  expect_true(grepl("tidyquant::tq_get", audit_text, fixed = TRUE))
  expect_true(grepl("RTL::expiry_table", audit_text, fixed = TRUE))
})
