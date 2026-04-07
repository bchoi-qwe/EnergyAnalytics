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
eia_key <- EnergyAnalytics:::ea_optional_env_var("EIA_API_KEY")
rtl_iuser <- EnergyAnalytics:::ea_optional_env_var("RTL_IUSER")
rtl_ipassword <- EnergyAnalytics:::ea_optional_env_var("RTL_IPASSWORD")

if (!nzchar(curl_bin)) {
  stop("System `curl` was not found on PATH.", call. = FALSE)
}


if (is.null(eia_key)) {
  stop("EIA_API_KEY is not set or still uses the example placeholder. Update .Renviron with a real EIA API key.", call. = FALSE)
}


cat("Project root: ", project_root, "\n", sep = "")
cat("R: ", R.version.string, "\n", sep = "")
cat("Platform: ", R.version$platform, "\n", sep = "")
cat("renv project: ", renv::project(), "\n", sep = "")
cat("Library paths:\n", sep = "")
writeLines(paste0(" - ", .libPaths()))
cat("curl: ", curl_bin, "\n", sep = "")
cat("EIA_API_KEY: ", ea_mask_secret(if (is.null(eia_key)) "" else eia_key), "\n", sep = "")
cat("RTL_IUSER: ", ea_mask_secret(if (is.null(rtl_iuser)) "" else rtl_iuser), "\n", sep = "")
cat("RTL_IPASSWORD: ", ea_mask_secret(if (is.null(rtl_ipassword)) "" else rtl_ipassword), "\n", sep = "")
cat("Package versions:\n", sep = "")
writeLines(paste0(
  " - ",
  package_versions,
  ": ",
  vapply(package_versions, function(pkg) as.character(utils::packageVersion(pkg)), character(1))
))

snapshot_manifest <- file.path(ea_latest_dir(project_root), "manifest.json")
if (file.exists(snapshot_manifest)) {
  ea_validate_snapshot_dir(ea_latest_dir(project_root))
  cat("Latest snapshot: valid\n", sep = "")
} else {
  cat("Latest snapshot: not built yet\n", sep = "")
}

if (is.null(rtl_iuser) || is.null(rtl_ipassword)) {
  stop("RTL_IUSER and RTL_IPASSWORD must be set. ZEMA contract prices and options are required for snapshot builds.", call. = FALSE)
}

EnergyAnalytics:::ea_validate_rtl_credentials(force = TRUE)
cat("ZEMA/RTL credentials: authenticated\n", sep = "")
