ea_script_file <- function() {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)

  if (length(file_arg) == 0L) {
    return(NULL)
  }

  normalizePath(
    sub("^--file=", "", file_arg[[1]]),
    winslash = "/",
    mustWork = TRUE
  )
}

ea_find_project_root <- function() {
  script_file <- ea_script_file()
  candidates <- c(
    if (!is.null(script_file)) dirname(script_file) else character(),
    getwd()
  )

  for (candidate in unique(candidates)) {
    current <- normalizePath(candidate, winslash = "/", mustWork = TRUE)

    repeat {
      if (file.exists(file.path(current, "DESCRIPTION"))) {
        return(current)
      }

      parent <- dirname(current)
      if (identical(parent, current)) {
        break
      }
      current <- parent
    }
  }

  stop("Could not locate the project root containing DESCRIPTION.", call. = FALSE)
}

ea_enter_project <- function(project_root = ea_find_project_root()) {
  project_root <- normalizePath(project_root, winslash = "/", mustWork = TRUE)
  setwd(project_root)
  options(EnergyAnalytics.project_root = project_root)

  profile_path <- file.path(project_root, ".Rprofile")
  if (file.exists(profile_path)) {
    source(profile_path, local = FALSE)
  }

  options(EnergyAnalytics.project_root = project_root)
  invisible(project_root)
}

ea_require_packages <- function(packages) {
  missing_packages <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]

  if (length(missing_packages) > 0L) {
    stop(
      "Missing required packages: ",
      paste(missing_packages, collapse = ", "),
      ". Run `Rscript scripts/bootstrap.R` first.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

ea_ensure_package <- function(package) {
  if (requireNamespace(package, quietly = TRUE)) {
    return(invisible(TRUE))
  }

  install.packages(package, repos = getOption("repos")[["CRAN"]])
  ea_require_packages(package)
  invisible(TRUE)
}

ea_load_dev_package <- function(project_root = ea_find_project_root()) {
  ea_require_packages("pkgload")
  pkgload::load_all(
    path = project_root,
    export_all = FALSE,
    helpers = FALSE,
    quiet = TRUE
  )

  invisible(TRUE)
}

ea_latest_dir <- function(project_root = ea_find_project_root()) {
  file.path(project_root, "inst", "extdata", "latest")
}

ea_release_dir <- function(project_root = ea_find_project_root()) {
  file.path(project_root, "artifacts", "releases")
}

ea_mask_secret <- function(value) {
  if (!nzchar(value)) {
    return("<unset>")
  }

  if (nchar(value) <= 6L) {
    return(strrep("*", nchar(value)))
  }

  paste0(substr(value, 1L, 3L), strrep("*", nchar(value) - 5L), substr(value, nchar(value) - 1L, nchar(value)))
}
