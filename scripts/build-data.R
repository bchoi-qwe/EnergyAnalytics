#!/usr/bin/env Rscript

script_file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_dir <- if (length(script_file_arg) > 0L) {
  dirname(normalizePath(sub("^--file=", "", script_file_arg[[1L]]), winslash = "/", mustWork = TRUE))
} else {
  normalizePath("scripts", winslash = "/", mustWork = TRUE)
}

source(file.path(script_dir, "_common.R"), local = TRUE)

project_root <- ea_enter_project()
ea_require_packages("pkgload")
ea_load_dev_package(project_root)

snapshot_id <- Sys.getenv("EA_SNAPSHOT_ID", unset = "")
manifest <- ea_build_latest_snapshot(
  latest_dir = ea_latest_dir(project_root),
  release_dir = ea_release_dir(project_root),
  snapshot_id = if (nzchar(snapshot_id)) snapshot_id else NULL
)

bundle_path <- file.path(
  ea_release_dir(project_root),
  paste0("energy-analytics-snapshot-", manifest$snapshot_id, ".tar.gz")
)

cat("Built snapshot: ", manifest$snapshot_id, "\n", sep = "")
cat("Latest dir: ", ea_latest_dir(project_root), "\n", sep = "")
cat("Release bundle: ", bundle_path, "\n", sep = "")
