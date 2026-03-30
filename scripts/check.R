#!/usr/bin/env Rscript

script_file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_dir <- if (length(script_file_arg) > 0L) {
  dirname(normalizePath(sub("^--file=", "", script_file_arg[[1L]]), winslash = "/", mustWork = TRUE))
} else {
  normalizePath("scripts", winslash = "/", mustWork = TRUE)
}

source(file.path(script_dir, "_common.R"), local = TRUE)

project_root <- ea_enter_project()
ea_require_packages(c("devtools", "pkgload"))
ea_load_dev_package(project_root)

snapshot_manifest <- file.path(ea_latest_dir(project_root), "manifest.json")
if (!file.exists(snapshot_manifest)) {
  stop(
    "No local snapshot found. Run `Rscript scripts/build-data.R` first.",
    call. = FALSE
  )
}

ea_validate_snapshot_dir(ea_latest_dir(project_root))
devtools::check(
  pkg = project_root,
  document = FALSE,
  manual = FALSE,
  error_on = "warning"
)

cat("devtools::check completed successfully for ", project_root, "\n", sep = "")
