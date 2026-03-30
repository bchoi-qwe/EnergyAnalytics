#!/usr/bin/env Rscript

options(repos = c(CRAN = "https://cloud.r-project.org"))

if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv", repos = getOption("repos")[["CRAN"]])
}

script_file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_dir <- if (length(script_file_arg) > 0L) {
  dirname(normalizePath(sub("^--file=", "", script_file_arg[[1L]]), winslash = "/", mustWork = TRUE))
} else {
  normalizePath("scripts", winslash = "/", mustWork = TRUE)
}

source(file.path(script_dir, "_common.R"), local = TRUE)

project_root <- ea_enter_project()
renv::consent(provided = TRUE)
renv::restore(project = project_root, prompt = FALSE)

cat("renv restore complete for ", project_root, "\n", sep = "")
