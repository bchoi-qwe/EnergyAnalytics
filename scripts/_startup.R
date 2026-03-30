ea_use_project_repos <- function() {
  repos <- getOption("repos")
  cran_repo <- repos[["CRAN"]]

  if (is.null(cran_repo) || identical(cran_repo, "@CRAN@") || !nzchar(cran_repo)) {
    options(repos = c(CRAN = "https://cloud.r-project.org"))
  }

  invisible(getOption("repos"))
}

ea_patch_quantmod_fred_curl <- function() {
  if (!requireNamespace("quantmod", quietly = TRUE)) return(invisible(FALSE))
  ns <- asNamespace("quantmod")
  current <- get("getSymbols.FRED", envir = ns)
  if (isTRUE(attr(current, "ea_curl_patch"))) return(invisible(TRUE))

  code <- utils::capture.output(print(current))
  code <- code[!grepl("^<bytecode:", code) & !grepl("^<environment:", code)]
  patched_str <- gsub("curl::curl(URL)", "URL", paste(code, collapse = "\n"), fixed = TRUE)
  
  patched <- eval(parse(text = patched_str))
  environment(patched) <- ns
  attr(patched, "ea_curl_patch") <- TRUE

  unlockBinding("getSymbols.FRED", ns)
  assign("getSymbols.FRED", patched, envir = ns)
  lockBinding("getSymbols.FRED", ns)

  invisible(TRUE)
}

ea_register_quantmod_fred_hook <- function() {
  if (isTRUE(getOption("EnergyAnalytics.quantmod_hook_registered"))) return(invisible(TRUE))
  setHook(packageEvent("quantmod", "onLoad"), function(...) ea_patch_quantmod_fred_curl(), action = "append")
  options(EnergyAnalytics.quantmod_hook_registered = TRUE)
  invisible(TRUE)
}

ea_activate_project_startup <- function(project_root = ".") {
  project_root <- normalizePath(project_root, winslash = "/", mustWork = TRUE)
  options(
    EnergyAnalytics.project_root = project_root,
    download.file.method = "libcurl"
  )
  ea_use_project_repos()
  ea_register_quantmod_fred_hook()

  if ("quantmod" %in% loadedNamespaces()) {
    ea_patch_quantmod_fred_curl()
  }

  invisible(project_root)
}
