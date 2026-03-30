ea_use_project_repos <- function() {
  repos <- getOption("repos")
  cran_repo <- repos[["CRAN"]]

  if (is.null(cran_repo) || identical(cran_repo, "@CRAN@") || !nzchar(cran_repo)) {
    options(repos = c(CRAN = "https://cloud.r-project.org"))
  }

  invisible(getOption("repos"))
}

ea_patch_quantmod_fred_http11 <- function() {
  if (!requireNamespace("quantmod", quietly = TRUE) || !nzchar(Sys.which("curl"))) {
    return(invisible(FALSE))
  }

  ns <- asNamespace("quantmod")
  current <- get("getSymbols.FRED", envir = ns)
  if (isTRUE(attr(current, "ea_http11_patch"))) {
    return(invisible(TRUE))
  }

  patched <- function(Symbols, env, return.class = "xts", ...) {
    importDefaults("getSymbols.FRED")
    this.env <- environment()
    for (var in names(list(...))) {
      assign(var, list(...)[[var]], this.env)
    }
    if (!hasArg("verbose")) {
      verbose <- FALSE
    }
    if (!hasArg("auto.assign")) {
      auto.assign <- TRUE
    }
    if (!hasArg("warnings")) {
      warnings <- TRUE
    }
    if (!hasArg("from")) {
      from <- ""
    }
    if (!hasArg("to")) {
      to <- ""
    }

    fred_url <- "https://fred.stlouisfed.org/graph/fredgraph.csv?id="
    return_sym <- Symbols
    no_data_sym <- NULL

    for (i in seq_along(Symbols)) {
      if (verbose) {
        cat("downloading ", Symbols[[i]], ".....\n\n")
      }

      test <- try({
        curl_bin <- Sys.which("curl")
        url <- paste0(fred_url, Symbols[[i]])
        csv_lines <- system2(
          curl_bin,
          args = c("--http1.1", "-fsSL", url),
          stdout = TRUE,
          stderr = TRUE
        )
        if (!is.null(attr(csv_lines, "status"))) {
          stop(paste(csv_lines, collapse = "\n"))
        }
        fr <- read.csv(text = paste(csv_lines, collapse = "\n"), na.strings = ".")
        if (verbose) {
          cat("done.\n")
        }
        fr <- xts::xts(
          as.matrix(fr[, -1]),
          as.Date(fr[, 1], origin = "1970-01-01"),
          src = "FRED",
          updated = Sys.time()
        )
        dim(fr) <- c(NROW(fr), 1)
        colnames(fr) <- as.character(toupper(Symbols[[i]]))
        fr <- fr[paste(from, to, sep = "/")]
        fr <- convert.time.series(fr = fr, return.class = return.class)
        Symbols[[i]] <- toupper(gsub("\\^", "", Symbols[[i]]))
        if (auto.assign) {
          assign(Symbols[[i]], fr, env)
        }
      }, silent = TRUE)

      if (inherits(test, "try-error")) {
        msg <- paste0(
          "Unable to import ",
          dQuote(return_sym[[i]]),
          ".\n",
          attr(test, "condition")$message
        )
        if (hasArg(".has1sym.") && match.call(expand.dots = TRUE)$.has1sym.) {
          stop(msg)
        }
        if (isTRUE(warnings)) {
          warning(msg, call. = FALSE, immediate. = TRUE)
        }
        no_data_sym <- c(no_data_sym, return_sym[[i]])
      }
    }

    if (auto.assign) {
      return(setdiff(return_sym, no_data_sym))
    }

    fr
  }

  attr(patched, "ea_http11_patch") <- TRUE
  environment(patched) <- ns

  unlockBinding("getSymbols.FRED", ns)
  assign("getSymbols.FRED", patched, envir = ns)
  lockBinding("getSymbols.FRED", ns)

  invisible(TRUE)
}

ea_register_quantmod_fred_hook <- function() {
  if (isTRUE(getOption("EnergyAnalytics.quantmod_hook_registered"))) {
    return(invisible(TRUE))
  }

  setHook(
    packageEvent("quantmod", "onLoad"),
    function(...) {
      ea_patch_quantmod_fred_http11()
    },
    action = "append"
  )

  options(EnergyAnalytics.quantmod_hook_registered = TRUE)
  invisible(TRUE)
}

ea_activate_project_startup <- function(project_root = ".") {
  project_root <- normalizePath(project_root, winslash = "/", mustWork = TRUE)
  options(EnergyAnalytics.project_root = project_root)
  ea_use_project_repos()
  ea_register_quantmod_fred_hook()

  if ("quantmod" %in% loadedNamespaces()) {
    ea_patch_quantmod_fred_http11()
  }

  invisible(project_root)
}
