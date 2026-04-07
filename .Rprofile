# Patch quantmod::getSymbols.FRED to use system curl with --http1.1
# FRED's server breaks with HTTP/2 via libcurl; system curl handles this.
local({
  patch_fred <- function() {
    if (!requireNamespace("quantmod", quietly = TRUE)) return()

    patched_fn <- function(Symbols, env, verbose = FALSE, auto.assign = TRUE,
                           warnings = TRUE, from = "", to = "", ...) {
      FRED.URL <- "https://fred.stlouisfed.org/graph/fredgraph.csv?id="
      returnSym <- Symbols
      noDataSym <- NULL

      for (i in seq_along(Symbols)) {
        test <- try({
          URL <- paste0(FRED.URL, Symbols[[i]])
          csv_text <- system(
            paste0("curl --http1.1 -s '", URL, "'"),
            intern = TRUE
          )
          if (length(csv_text) < 2) stop("no data returned")
          fr <- utils::read.csv(textConnection(csv_text), na.strings = ".")
          fr[, 1] <- as.Date(fr[, 1])
          colnames(fr) <- c("Date", toupper(Symbols[[i]]))
          fr <- xts::xts(fr[, -1, drop = FALSE], order.by = fr[, 1])
          colnames(fr) <- toupper(Symbols[[i]])
          fr
        }, silent = TRUE)

        if (inherits(test, "try-error")) {
          if (warnings) warning(paste("Unable to import", Symbols[[i]]))
          noDataSym <- c(noDataSym, Symbols[[i]])
          next
        }

        if (auto.assign) {
          assign(toupper(Symbols[[i]]), test, env)
        } else {
          returnSym <- test
        }
      }

      if (auto.assign) return(returnSym)
      return(returnSym)
    }

    utils::assignInNamespace("getSymbols.FRED", patched_fn, ns = "quantmod")
  }

  setHook(packageEvent("quantmod", "onLoad"), function(...) patch_fred())
  if ("quantmod" %in% loadedNamespaces()) patch_fred()
})
