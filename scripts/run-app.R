#!/usr/bin/env Rscript

script_file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_dir <- if (length(script_file_arg) > 0L) {
  dirname(normalizePath(sub("^--file=", "", script_file_arg[[1L]]), winslash = "/", mustWork = TRUE))
} else {
  normalizePath("scripts", winslash = "/", mustWork = TRUE)
}

source(file.path(script_dir, "_common.R"), local = TRUE)

project_root <- ea_enter_project()

if (!requireNamespace("EnergyAnalytics", quietly = TRUE)) {
  ea_load_dev_package(project_root)
}

host <- Sys.getenv("SHINY_HOST", unset = "0.0.0.0")
port_value <- Sys.getenv("PORT", unset = Sys.getenv("SHINY_PORT", unset = "8080"))
port <- suppressWarnings(as.integer(port_value))

if (!nzchar(host)) {
  stop("SHINY_HOST must not be empty.", call. = FALSE)
}

if (is.na(port) || port < 1L || port > 65535L) {
  stop("PORT or SHINY_PORT must be an integer between 1 and 65535.", call. = FALSE)
}

options(
  shiny.host = host,
  shiny.port = port
)

cat("Starting EnergyAnalytics on http://", host, ":", port, "\n", sep = "")

shiny::runApp(
  getExportedValue("EnergyAnalytics", "run_app")(),
  host = host,
  port = port,
  launch.browser = FALSE
)
