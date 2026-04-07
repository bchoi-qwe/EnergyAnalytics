#!/usr/bin/env Rscript

options(repos = c(CRAN = "https://cloud.r-project.org"))

script_file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_dir <- if (length(script_file_arg) > 0L) {
  dirname(normalizePath(sub("^--file=", "", script_file_arg[[1L]]), winslash = "/", mustWork = TRUE))
} else {
  normalizePath("scripts", winslash = "/", mustWork = TRUE)
}

source(file.path(script_dir, "_common.R"), local = TRUE)

project_root <- ea_enter_project()
lockfile_path <- file.path(project_root, "renv.lock")

ea_ensure_package("renv")

if (file.exists(lockfile_path)) {
  renv::consent(provided = TRUE)
  renv::restore(project = project_root, prompt = FALSE)
  cat("renv restore complete for ", project_root, "\n", sep = "")
} else {
  ea_ensure_package("pak")
  pak::local_install_dev_deps(
    root = project_root,
    upgrade = FALSE,
    ask = FALSE,
    dependencies = TRUE
  )
  pak::local_install(
    root = project_root,
    upgrade = FALSE,
    ask = FALSE,
    dependencies = NA
  )

  cat(
    "No renv.lock found; installed project dependencies from DESCRIPTION with pak for ",
    project_root,
    "\n",
    sep = ""
  )
}
