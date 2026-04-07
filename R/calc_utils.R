# ---- Shared Calculation Utilities ----
# Common helpers used by all calc_*.R modules.

ea_latest_snapshot_date <- function() {
  curves <- ea_load_dataset("commodity_curve_long")
  max(curves$date, na.rm = TRUE)
}

ea_load_filtered_curves <- function(filters, apply_date_range = TRUE) {
  curves <- ea_load_dataset("commodity_curve_long")
  markets <- ea_coalesce(filters$commodities, c("CL", "BRN", "RB"))
  expiry_lo <- ea_coalesce(filters$expiry_range[1], 1L)
  expiry_hi <- ea_coalesce(filters$expiry_range[2], 12L)

  out <- curves %>%
    dplyr::filter(
      .data$market %in% markets,
      .data$curve_point_num >= expiry_lo,
      .data$curve_point_num <= expiry_hi
    )

  if (isTRUE(apply_date_range) && !is.null(filters$date_range) && length(filters$date_range) == 2L) {
    out <- out %>%
      dplyr::filter(
        .data$date >= as.Date(filters$date_range[1]),
        .data$date <= as.Date(filters$date_range[2])
      )
  }

  out %>% dplyr::arrange(.data$market, .data$curve_point_num, .data$date)
}

ea_filter_date_range <- function(data, date_range = NULL) {
  if (!is.data.frame(data) || nrow(data) == 0L || !"date" %in% names(data)) {
    return(data)
  }

  if (is.null(date_range) || length(date_range) != 2L || any(is.na(date_range))) {
    return(data)
  }

  start_date <- as.Date(date_range[[1]])
  end_date <- as.Date(date_range[[2]])

  data %>%
    dplyr::filter(.data$date >= start_date, .data$date <= end_date)
}

ea_resolve_analysis_windows <- function(display_dates, all_dates, history_context = "5y") {
  display_dates <- as.Date(display_dates)
  all_dates <- as.Date(all_dates)

  display_dates <- display_dates[!is.na(display_dates)]
  all_dates <- all_dates[!is.na(all_dates)]

  if (length(display_dates) == 0L || length(all_dates) == 0L) {
    return(list(
      display_window = as.Date(c(NA, NA)),
      context_window = as.Date(c(NA, NA)),
      anchor_date = as.Date(NA),
      context_start = as.Date(NA)
    ))
  }

  anchor_date <- max(display_dates, na.rm = TRUE)
  display_window <- as.Date(c(min(display_dates, na.rm = TRUE), anchor_date))
  context_start <- ea_history_context_start_date(
    end_date = anchor_date,
    history_context = history_context,
    min_date = min(all_dates, na.rm = TRUE)
  )

  list(
    display_window = display_window,
    context_window = as.Date(c(context_start, anchor_date)),
    anchor_date = anchor_date,
    context_start = context_start
  )
}

ea_slice_history_context <- function(data, end_date, history_context = "5y") {
  if (!is.data.frame(data) || nrow(data) == 0L || !"date" %in% names(data)) {
    return(data)
  }

  if (!inherits(end_date, "Date") || length(end_date) != 1L || is.na(end_date)) {
    return(data[0, , drop = FALSE])
  }

  context_start <- ea_history_context_start_date(
    end_date = end_date,
    history_context = history_context,
    min_date = min(as.Date(data$date), na.rm = TRUE)
  )

  data %>%
    dplyr::filter(.data$date >= context_start, .data$date <= end_date)
}

ea_complete_market_frame <- function(wide_frame, markets, date_col = "date") {
  if (!is.data.frame(wide_frame) || length(markets) == 0L) {
    return(tibble::tibble())
  }

  required_cols <- c(date_col, markets)
  if (!all(required_cols %in% names(wide_frame))) {
    return(tibble::tibble())
  }

  wide_frame %>%
    dplyr::select(dplyr::all_of(required_cols)) %>%
    dplyr::filter(stats::complete.cases(dplyr::pick(dplyr::all_of(markets))))
}

ea_simple_regression_stats <- function(x, y, min_obs = 20L) {
  valid <- is.finite(x) & is.finite(y)
  x_valid <- x[valid]
  y_valid <- y[valid]
  n_obs <- length(x_valid)

  empty <- list(
    n_obs = n_obs,
    alpha = NA_real_,
    beta = NA_real_,
    r_squared = NA_real_,
    std_error = NA_real_,
    residuals = numeric(),
    valid = valid
  )

  if (n_obs < min_obs) {
    return(empty)
  }

  sx <- sum(x_valid)
  sy <- sum(y_valid)
  sxx <- sum(x_valid * x_valid)
  syy <- sum(y_valid * y_valid)
  sxy <- sum(x_valid * y_valid)

  ssx <- sxx - (sx * sx) / n_obs
  ssy <- syy - (sy * sy) / n_obs
  sxy_c <- sxy - (sx * sy) / n_obs

  if (!is.finite(ssx) || !is.finite(ssy) || !is.finite(sxy_c) || ssx <= 0) {
    return(empty)
  }

  beta <- sxy_c / ssx
  alpha <- (sy - beta * sx) / n_obs
  residuals <- y_valid - (alpha + beta * x_valid)
  sse <- sum(residuals^2)
  r_squared <- if (is.finite(ssy) && ssy > 0) {
    max(min(1 - (sse / ssy), 1), 0)
  } else {
    NA_real_
  }
  std_error <- if (n_obs > 2L && is.finite(sse)) {
    sqrt((sse / (n_obs - 2L)) / ssx)
  } else {
    NA_real_
  }

  list(
    n_obs = n_obs,
    alpha = alpha,
    beta = beta,
    r_squared = r_squared,
    std_error = std_error,
    residuals = residuals,
    valid = valid
  )
}

ea_ou_stationary_sd <- function(theta, sigma) {
  if (!is.finite(theta) || !is.finite(sigma) || theta <= 0) {
    return(NA_real_)
  }

  sigma / sqrt(2 * theta)
}

ea_ou_half_life_days <- function(theta, trading_days = 252) {
  if (!is.finite(theta) || theta <= 0 || !is.finite(trading_days) || trading_days <= 0) {
    return(NA_real_)
  }

  log(2) * trading_days / theta
}

ea_normalize_history_context <- function(history_context = NULL) {
  if (is.list(history_context)) {
    mode <- tolower(trimws(as.character(ea_coalesce(history_context$mode, "trailing"))[[1]]))
    if (identical(mode, "full")) {
      return("full")
    }

    years <- suppressWarnings(as.integer(ea_coalesce(history_context$years, 5L)[[1]]))
    if (!is.finite(years) || is.na(years) || years < 1L) {
      years <- 5L
    }

    return(paste0(years, "y"))
  }

  if (is.numeric(history_context) && length(history_context) == 1L && !is.na(history_context)) {
    years <- as.integer(round(history_context[[1]]))
    years <- max(years, 1L)
    return(paste0(years, "y"))
  }

  raw_value <- ea_coalesce(history_context, "5y")
  current <- tolower(trimws(as.character(raw_value[[1]])))

  alias_map <- c(
    "full" = "full",
    "all" = "full",
    "full history" = "full",
    "5y" = "5y",
    "5yr" = "5y",
    "5 years" = "5y",
    "trailing 5y" = "5y",
    "3y" = "3y",
    "3yr" = "3y",
    "3 years" = "3y",
    "trailing 3y" = "3y"
  )

  normalized <- unname(alias_map[current])
  if (length(normalized) == 1L && !is.na(normalized)) {
    return(normalized)
  }

  current <- gsub("\\s+", "", current)
  trailing_match <- regexec("^(?:trailing)?([0-9]{1,2})y(?:ears?)?$", current)
  trailing_capture <- regmatches(current, trailing_match)[[1]]
  if (length(trailing_capture) >= 2L) {
    years <- suppressWarnings(as.integer(trailing_capture[[2]]))
    if (is.finite(years) && !is.na(years) && years >= 1L) {
      return(paste0(years, "y"))
    }
  }

  "5y"
}

ea_history_context_label <- function(history_context = NULL) {
  normalized <- ea_normalize_history_context(history_context)
  if (identical(normalized, "full")) {
    return("Full")
  }

  years <- suppressWarnings(as.integer(sub("y$", "", normalized)))
  if (!is.finite(years) || is.na(years) || years < 1L) {
    years <- 5L
  }

  paste0(years, "Y")
}

ea_history_context_start_date <- function(end_date, history_context = NULL, min_date = NULL) {
  if (!inherits(end_date, "Date") || length(end_date) == 0L || is.na(end_date)) {
    return(as.Date(NA))
  }

  normalized <- ea_normalize_history_context(history_context)
  start_date <- if (identical(normalized, "full")) {
    as.Date("1900-01-01")
  } else {
    years <- suppressWarnings(as.integer(sub("y$", "", normalized)))
    if (!is.finite(years) || is.na(years) || years < 1L) {
      years <- 5L
    }
    end_date - 365L * years
  }

  if (!is.null(min_date) && length(min_date) == 1L && inherits(min_date, "Date") && !is.na(min_date)) {
    start_date <- max(start_date, min_date)
  }

  start_date
}

ea_log_returns <- function(curves_long) {
  curves_long %>%
    dplyr::arrange(.data$market, .data$curve_point, .data$date) %>%
    dplyr::group_by(.data$market, .data$curve_point) %>%
    dplyr::mutate(log_return = suppressWarnings(log(.data$value / dplyr::lag(.data$value)))) %>%
    dplyr::ungroup()
}

ea_curve_to_wide <- function(curves_long) {
  curves_long %>%
    dplyr::select("date", "market", "curve_point", "value") %>%
    tidyr::pivot_wider(names_from = "curve_point", values_from = "value")
}
