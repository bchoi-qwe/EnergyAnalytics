setup_script <- if (file.exists(file.path("data-raw", "00_setup.R"))) {
  file.path("data-raw", "00_setup.R")
} else {
  "00_setup.R"
}

source(setup_script)

latest_dir <- file.path(project_root, "inst", "extdata", "latest")
dir.create(latest_dir, recursive = TRUE, showWarnings = FALSE)

arrow::write_feather(
  ea_build_ust_curve_long(),
  file.path(latest_dir, "ust_curve_long.feather"),
  compression = "zstd"
)
