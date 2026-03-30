setup_script <- if (file.exists(file.path("data-raw", "00_setup.R"))) {
  file.path("data-raw", "00_setup.R")
} else {
  "00_setup.R"
}

source(setup_script)

latest_dir <- file.path(project_root, "inst", "extdata", "latest")
dir.create(latest_dir, recursive = TRUE, showWarnings = FALSE)

arrow::write_feather(
  ea_build_eia_stocks_long(),
  file.path(latest_dir, "eia_stocks_long.feather"),
  compression = "zstd"
)

arrow::write_feather(
  ea_build_eia_storage_capacity_long(),
  file.path(latest_dir, "eia_storage_capacity_long.feather"),
  compression = "zstd"
)
