# data-raw/06_build_options_base.R
# Pipeline to process ZEMA option quotes, calculate IV/Greeks, and save to feather.

source("data-raw/00_setup.R")
# Ensure we have all exports including internals
pkgload::load_all(export_all = TRUE, quiet = TRUE)

options_surface <- ea_build_options_surface_long()

if (nrow(options_surface) > 0) {
  arrow::write_feather(
    options_surface,
    file.path(ea_latest_dir(), "options_surface_long.feather"),
    compression = "zstd"
  )
  message("Options surface build complete: ", nrow(options_surface), " rows saved.")
} else {
  stop("No options data was generated. Snapshot builds require non-empty ZEMA options output.", call. = FALSE)
}
