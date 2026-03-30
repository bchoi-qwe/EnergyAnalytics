setup_script <- if (file.exists(file.path("data-raw", "00_setup.R"))) {
  file.path("data-raw", "00_setup.R")
} else {
  "00_setup.R"
}

source(setup_script)

latest_dir <- file.path(project_root, "inst", "extdata", "latest")
dir.create(latest_dir, recursive = TRUE, showWarnings = FALSE)

arrow::write_feather(
  ea_build_commodity_curve_long(),
  file.path(latest_dir, "commodity_curve_long.feather"),
  compression = "zstd"
)

arrow::write_feather(
  ea_build_commodity_curve_wide(),
  file.path(latest_dir, "commodity_curve_wide.feather"),
  compression = "zstd"
)

arrow::write_feather(
  ea_build_contract_expiry_metadata(),
  file.path(latest_dir, "contract_expiry_metadata.feather"),
  compression = "zstd"
)
