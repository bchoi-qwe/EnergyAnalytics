#!/usr/bin/env Rscript

script_file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_dir <- if (length(script_file_arg) > 0L) {
  dirname(normalizePath(sub("^--file=", "", script_file_arg[[1L]]), winslash = "/", mustWork = TRUE))
} else {
  normalizePath("scripts", winslash = "/", mustWork = TRUE)
}

source(file.path(script_dir, "_common.R"), local = TRUE)

project_root <- ea_enter_project()
ea_require_packages(c("arrow", "devtools", "pkgload", "quantmod", "renv", "RTL", "testthat", "tidyquant"))
ea_load_dev_package(project_root)

package_versions <- c("renv", "RTL", "tidyquant", "quantmod", "arrow", "pkgload", "devtools", "testthat")
curl_bin <- Sys.which("curl")
eia_key <- Sys.getenv("EIA_API_KEY", unset = "")
fred_patch_applied <- isTRUE(
  attr(
    get("getSymbols.FRED", envir = asNamespace("quantmod")),
    "ea_http11_patch"
  )
)

if (!nzchar(curl_bin)) {
  stop("System `curl` was not found on PATH.", call. = FALSE)
}

if (!fred_patch_applied) {
  stop("The quantmod FRED HTTP/1.1 patch is not active.", call. = FALSE)
}

if (!nzchar(eia_key)) {
  stop("EIA_API_KEY is not set. Copy .Renviron.example to .Renviron and fill it in.", call. = FALSE)
}

fred_smoke <- tidyquant::tq_get("DGS10", get = "economic.data")
if (!is.data.frame(fred_smoke) || nrow(fred_smoke) == 0L) {
  stop("FRED smoke test returned no rows.", call. = FALSE)
}

cat("Project root: ", project_root, "\n", sep = "")
cat("R: ", R.version.string, "\n", sep = "")
cat("Platform: ", R.version$platform, "\n", sep = "")
cat("renv project: ", renv::project(), "\n", sep = "")
cat("Library paths:\n", sep = "")
writeLines(paste0(" - ", .libPaths()))
cat("curl: ", curl_bin, "\n", sep = "")
cat("EIA_API_KEY: ", ea_mask_secret(eia_key), "\n", sep = "")
cat("Package versions:\n", sep = "")
writeLines(paste0(
  " - ",
  package_versions,
  ": ",
  vapply(package_versions, function(pkg) as.character(utils::packageVersion(pkg)), character(1))
))
cat(
  "FRED smoke test: ",
  nrow(fred_smoke),
  " rows, ",
  as.character(min(fred_smoke$date)),
  " -> ",
  as.character(max(fred_smoke$date)),
  "\n",
  sep = ""
)

snapshot_manifest <- file.path(ea_latest_dir(project_root), "manifest.json")
if (file.exists(snapshot_manifest)) {
  ea_validate_snapshot_dir(ea_latest_dir(project_root))
  cat("Latest snapshot: valid\n", sep = "")
} else {
  cat("Latest snapshot: not built yet\n", sep = "")
}
