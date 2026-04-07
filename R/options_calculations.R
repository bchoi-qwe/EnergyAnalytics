# ---- Options Greeks Calculation Pipeline ----
# Bridges canonical Feather datasets to the Rcpp Black-76 engine.

#' @useDynLib EnergyAnalytics, .registration = TRUE
#' @importFrom Rcpp sourceCpp
NULL

ea_interpolate_risk_free <- function(ust_curve, days_to_expiry, target_date = NULL) {
  target_date <- target_date %||% max(ust_curve$date, na.rm = TRUE)

  ust_snapshot <- ust_curve %>%
    dplyr::filter(.data$date == as.Date(target_date)) %>%
    dplyr::arrange(.data$curve_point_num) %>%
    dplyr::select(.data$curve_point_num, .data$value)

  if (nrow(ust_snapshot) == 0L) {
    cli::cli_abort("UST curve snapshot is missing for options date {.val {as.character(as.Date(target_date))}}.")
  }

  tenors_years <- ust_snapshot$curve_point_num
  rates <- ust_snapshot$value / 100

  vapply(
    days_to_expiry,
    function(dte) {
      t_years <- dte / 365.25
      if (t_years <= min(tenors_years)) return(rates[1])
      if (t_years >= max(tenors_years)) return(rates[length(rates)])
      stats::approx(x = tenors_years, y = rates, xout = t_years, rule = 2)$y
    },
    numeric(1)
  )
}

ea_zema_option_spec_path <- function() {
  file.path(ea_project_root(), "data-raw", "zema_options_contracts.csv")
}

ea_zema_cme_options_spec <- function(contract_prices) {
  empty_result <- tibble::tibble(
    market = character(),
    feed = character(),
    contract = character(),
    contract_month = character(),
    strike = numeric(),
    option_type = character()
  )

  if (!is.data.frame(contract_prices) || nrow(contract_prices) == 0L) {
    return(empty_result)
  }

  product_spec <- tibble::tribble(
    ~market, ~feed, ~product_code, ~strike_scale, ~strike_increment, ~atm_steps_each_side, ~max_contracts,
    "CL", "CME_NymexOptions_EOD", "LO", 100, 1.0, 2L, 3L,
    "NG", "CME_NymexOptions_EOD", "ON", 1000, 0.05, 2L, 3L,
    "HO", "CME_NymexOptions_EOD", "OH", 10000, 0.01, 2L, 3L,
    "RB", "CME_NymexOptions_EOD", "OB", 10000, 0.01, 2L, 3L
  )

  target_contracts <- contract_prices %>%
    dplyr::filter(.data$market %in% product_spec$market, .data$last_trade >= .data$date) %>%
    dplyr::group_by(.data$market) %>%
    dplyr::filter(.data$date == max(.data$date, na.rm = TRUE)) %>%
    dplyr::arrange(.data$last_trade, .data$contract_year, .data$contract_month, .by_group = TRUE) %>%
    dplyr::mutate(curve_point_num = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(
      .data$date,
      .data$market,
      curve_point_num = as.numeric(.data$curve_point_num),
      forward = as.numeric(.data$value),
      contract_year = as.integer(.data$contract_year),
      contract_month_num = as.integer(.data$contract_month),
      contract_month_letter = as.character(.data$contract_month_letter),
      contract_month = sprintf("%04d-%02d", .data$contract_year, .data$contract_month),
      last_trade = as.Date(.data$last_trade)
    ) %>%
    dplyr::inner_join(product_spec, by = "market") %>%
    dplyr::filter(.data$curve_point_num <= .data$max_contracts, .data$forward > 0) %>%
    dplyr::arrange(.data$market, .data$curve_point_num)

  if (nrow(target_contracts) == 0L) {
    return(empty_result)
  }

  purrr::pmap_dfr(target_contracts, function(date, market, curve_point_num, forward, contract_year,
                                             contract_month_num, contract_month_letter, contract_month, last_trade, feed,
                                             product_code, strike_scale, strike_increment,
                                             atm_steps_each_side, max_contracts) {
    atm_strike <- round(forward / strike_increment) * strike_increment
    strike_offsets <- seq.int(-atm_steps_each_side, atm_steps_each_side) * strike_increment
    strikes <- sort(unique(round(atm_strike + strike_offsets, 6)))
    strikes <- strikes[strikes > 0]
    if (length(strikes) == 0L) {
      return(empty_result)
    }

    tidyr::expand_grid(
      strike = strikes,
      option_type = c("C", "P")
    ) %>%
      dplyr::transmute(
        market = market,
        feed = feed,
        contract = sprintf(
          "@%s%02d%s%s%d",
          product_code,
          contract_year %% 100L,
          contract_month_letter,
          option_type,
          as.integer(round(strike * strike_scale))
        ),
        contract_month = contract_month,
        strike = round(strike, 6),
        option_type = option_type
      )
  }) %>%
    dplyr::distinct()
}

ea_build_zema_options_raw <- function(contract_prices = NULL, spec_path = NULL, from = NULL) {
  empty_result <- tibble::tibble(
    obs_date = as.Date(character()),
    market = character(),
    contract_month = character(),
    strike = numeric(),
    type = character(),
    premium = numeric(),
    source_feed = character(),
    source_series = character()
  )

  ea_rtl_credentials(required = TRUE)
  ea_validate_rtl_credentials()

  contract_prices <- contract_prices %||% ea_load_dataset("commodity_contract_prices_long")
  spec_path <- spec_path %||% ea_zema_option_spec_path()

  auto_spec <- ea_zema_cme_options_spec(
    contract_prices = contract_prices
  )

  manual_spec <- if (file.exists(spec_path)) {
    option_spec <- readr::read_csv(spec_path, show_col_types = FALSE)
    required_cols <- c("market", "feed", "contract", "contract_month", "strike", "option_type")
    missing_cols <- setdiff(required_cols, names(option_spec))
    if (length(missing_cols) > 0L) {
      cli::cli_abort(
        "{.file {basename(spec_path)}} is missing required columns: {.val {missing_cols}}."
      )
    }
    option_spec
  } else {
    tibble::tibble(
      market = character(),
      feed = character(),
      contract = character(),
      contract_month = character(),
      strike = numeric(),
      option_type = character()
    )
  }

  option_spec <- dplyr::bind_rows(auto_spec, manual_spec) %>%
    dplyr::transmute(
      market = as.character(.data$market),
      feed = as.character(.data$feed),
      contract = as.character(.data$contract),
      contract_code = sub("^@", "", as.character(.data$contract)),
      contract_month = as.character(.data$contract_month),
      strike = as.numeric(.data$strike),
      type = stringr::str_sub(stringr::str_to_upper(as.character(.data$option_type)), 1L, 1L)
    ) %>%
    dplyr::filter(!is.na(.data$strike), .data$type %in% c("C", "P")) %>%
    dplyr::distinct()

  if (nrow(option_spec) == 0L) {
    cli::cli_abort("No ZEMA option contracts were specified or derived for {.val options_surface_long}.")
  }

  creds <- ea_rtl_credentials(required = TRUE)
  from <- as.character(as.Date(from %||% (max(contract_prices$date, na.rm = TRUE) - 14)))

  quote_groups <- option_spec %>%
    dplyr::distinct(.data$feed, .data$contract) %>%
    dplyr::group_by(.data$feed) %>%
    dplyr::group_split()

  quotes_long <- purrr::map_dfr(quote_groups, function(feed_group) {
    ea_zema_fetch_prices_long(
      feed = feed_group$feed[[1]],
      contracts = feed_group$contract,
      from = from,
      iuser = creds$iuser,
      ipassword = creds$ipassword,
      allow_partial = TRUE
    )
  })

  if (nrow(quotes_long) == 0L) {
    cli::cli_abort("ZEMA returned no options quotes for {.val options_surface_long}.")
  }

  quotes_long %>%
    dplyr::inner_join(
      option_spec %>%
        dplyr::select(
          .data$market,
          .data$feed,
          .data$contract_code,
          .data$contract_month,
          .data$strike,
          .data$type
        ),
      by = c("feed", "contract_code")
    ) %>%
    dplyr::transmute(
      obs_date = as.Date(.data$date),
      .data$market,
      .data$contract_month,
      strike = as.numeric(.data$strike),
      type = .data$type,
      premium = as.numeric(.data$value),
      source_feed = .data$feed,
      source_series = .data$contract_code
    )
}

#' Build the Unified Options Cube
#' @export
ea_build_options_surface_long <- function(contract_prices = NULL,
                                          ust_curve = NULL) {
  # Schema definition for consistency in empty returns
  empty_result <- tibble::tibble(
    date = as.Date(character()),
    market = character(),
    curve_point_num = numeric(),
    contract_month = character(),
    days_to_expiry = numeric(),
    strike = numeric(),
    option_type = character(),
    underlying_price = numeric(),
    implied_volatility = numeric(),
    delta = numeric(),
    gamma = numeric(),
    vega = numeric(),
    theta = numeric(),
    rho = numeric(),
    vanna = numeric(),
    charm = numeric(),
    speed = numeric(),
    zomma = numeric(),
    color = numeric(),
    ultima = numeric(),
    source = character(),
    source_family = character(),
    source_feed = character(),
    source_series = character()
  )

  # Use provided dependencies or load from disk
  contract_prices <- contract_prices %||% ea_load_dataset("commodity_contract_prices_long")
  ust_curve <- ust_curve %||% ea_load_dataset("ust_curve_long")

  raw_options <- ea_build_zema_options_raw(
    contract_prices = contract_prices,
    from = max(contract_prices$date, na.rm = TRUE) - 14
  )

  raw_options <- raw_options %>%
    dplyr::mutate(
      obs_date = as.Date(.data$obs_date),
      strike = as.numeric(.data$strike),
      premium = as.numeric(.data$premium),
      type = stringr::str_sub(stringr::str_to_upper(as.character(.data$type)), 1L, 1L)
    ) %>%
    dplyr::filter(!is.na(.data$obs_date), !is.na(.data$strike), !is.na(.data$premium), .data$type %in% c("C", "P"))

  contract_prices_mapped <- contract_prices %>%
    dplyr::group_by(.data$date, .data$market) %>%
    dplyr::arrange(.data$last_trade, .data$contract_year, .data$contract_month, .by_group = TRUE) %>%
    dplyr::mutate(curve_point_num = as.numeric(dplyr::row_number())) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(
      date = as.Date(.data$date),
      .data$market,
      .data$curve_point_num,
      contract_month = sprintf("%04d-%02d", .data$contract_year, .data$contract_month),
      underlying_price = as.numeric(.data$value),
      last_trade = as.Date(.data$last_trade)
    )

  # 3. Join Raw Options to Mapped Futures
  options_prep <- raw_options %>%
    dplyr::inner_join(contract_prices_mapped, by = c("obs_date" = "date", "market", "contract_month")) %>%
    dplyr::mutate(
      dte = pmax(as.numeric(difftime(.data$last_trade, .data$obs_date, units = "days")), 1),
      T_years = .data$dte / 365.25
    )

  if (nrow(options_prep) == 0L) return(empty_result)

  # 4. Calculate Risk Free Rates
  options_prep$risk_free <- purrr::map2_dbl(
    options_prep$dte, options_prep$obs_date,
    ~ ea_interpolate_risk_free(ust_curve, .x, .y)
  )

  # 4. Calculate IV (Newton-Raphson)
  calc_iv <- function(F, K, r, T, premium, is_call) {
    sig <- rep(0.3, length(F))
    for (i in 1:15) {
      g <- black76_greeks_vectorised(F, K, r, T, sig, is_call)
      diff <- g$premium - premium
      idx <- !is.na(diff) & abs(diff) > 1e-6
      if (!any(idx)) break
      update_idx <- idx & !is.na(g$vega) & g$vega > 1e-8
      if (!any(update_idx)) break
      sig[update_idx] <- sig[update_idx] - (diff[update_idx] / g$vega[update_idx])
      sig <- pmax(pmin(sig, 5.0), 0.001)
    }
    sig
  }

  options_prep$iv <- calc_iv(
    options_prep$underlying_price, options_prep$strike, options_prep$risk_free,
    options_prep$T_years, options_prep$premium, options_prep$type == "C"
  )

  # 5. Calculate All Greeks
  greeks <- black76_greeks_vectorised(
    options_prep$underlying_price, options_prep$strike, options_prep$risk_free,
    options_prep$T_years, options_prep$iv, options_prep$type == "C"
  )

  # 6. Final Formatting
  options_prep %>%
    dplyr::mutate(
      implied_volatility = .data$iv,
      delta = greeks$delta,
      gamma = greeks$gamma,
      vega = greeks$vega,
      theta = greeks$theta,
      rho = greeks$rho,
      vanna = greeks$vanna,
      charm = greeks$charm,
      speed = greeks$speed,
      zomma = greeks$zomma,
      color = greeks$color,
      ultima = greeks$ultima,
      source = "ZEMA",
      source_family = "RTL::getPrices"
    ) %>%
    dplyr::select(
      date = .data$obs_date,
      .data$market,
      .data$curve_point_num,
      .data$contract_month,
      days_to_expiry = .data$dte,
      .data$strike,
      option_type = .data$type,
      .data$underlying_price,
      .data$implied_volatility,
      .data$delta,
      .data$gamma,
      .data$vega,
      .data$theta,
      .data$rho,
      .data$vanna,
      .data$charm,
      .data$speed,
      .data$zomma,
      .data$color,
      .data$ultima,
      .data$source,
      .data$source_family,
      .data$source_feed,
      .data$source_series
    )
}

ea_calc_surface_greeks <- function(filters, history_context = "full") {
  catalog <- ea_market_catalog()
  history_context <- ea_normalize_history_context(history_context)
  markets <- ea_coalesce(filters$commodities, ea_global_filter_defaults(catalog)$commodities)
  labels <- stats::setNames(catalog$label, catalog$market)

  expiry_lo <- ea_coalesce(filters$expiry_range[1], 1L)
  expiry_hi <- ea_coalesce(filters$expiry_range[2], 12L)
  atm_tolerance <- 0.05

  empty_kpis <- tibble::tibble(title = character(), value = character(), delta = character(), icon = character(), status = character())
  greeks_empty <- list(
    available_markets = character(),
    history_context = history_context,
    history_context_label = ea_history_context_label(history_context),
    display_window = as.Date(c(NA, NA)),
    context_window = as.Date(c(NA, NA)),
    latest_surface_date = as.Date(NA),
    kpis = empty_kpis,
    full_grid = tibble::tibble(),
    cross_greeks_vanna = tibble::tibble(curve_point_num = numeric(), moneyness = numeric(), vanna = numeric()),
    cross_greeks_charm = tibble::tibble(curve_point_num = numeric(), moneyness = numeric(), charm = numeric()),
    term_greeks = tibble::tibble(market = character(), label = character(), curve_point = character(), curve_point_num = numeric(), gamma = numeric(), vega = numeric()),
    strike_profile = tibble::tibble(moneyness = numeric(), strike = numeric(), speed = numeric(), zomma = numeric()),
    greeks_concentration = tibble::tibble(market = character(), tenor_bucket = character(), delta = numeric(), gamma = numeric(), vega = numeric(), theta = numeric()),
    pnl_grid = tibble::tibble(spot_shock = numeric(), vol_shock = numeric(), pnl = numeric()),
    skew_ratio = tibble::tibble(market = character(), curve_point_num = numeric(), atm_iv = numeric(), otm_put_iv = numeric(), skew_ratio = numeric()),
    atm_history = tibble::tibble(date = as.Date(character()), market = character(), curve_point_num = numeric(), atm_iv = numeric(), skew_ratio = numeric(), gamma = numeric(), vega = numeric()),
    surface_context = tibble::tibble(market = character(), atm_iv = numeric(), iv_percentile = numeric(), iv_zscore = numeric(), skew_ratio = numeric(), skew_percentile = numeric(), skew_zscore = numeric()),
    notes = character(),
    assumptions = character()
  )

  vol_surface <- ea_load_dataset("options_surface_long")
  ust_curve <- ea_load_dataset("ust_curve_long")

  if (nrow(vol_surface) == 0L || !"curve_point_num" %in% names(vol_surface)) {
    return(greeks_empty)
  }

  all_surface <- vol_surface %>%
    dplyr::filter(
      .data$market %in% markets,
      .data$curve_point_num >= expiry_lo,
      .data$curve_point_num <= expiry_hi
    ) %>%
    dplyr::mutate(
      forward = .data$underlying_price,
      iv = .data$implied_volatility,
      T_years = .data$days_to_expiry / 365.25,
      label = labels[.data$market],
      moneyness = dplyr::if_else(
        !is.na(.data$underlying_price) & .data$underlying_price != 0,
        .data$strike / .data$underlying_price,
        NA_real_
      )
    ) %>%
    dplyr::arrange(.data$market, .data$date, .data$curve_point_num, .data$strike)

  if (nrow(all_surface) == 0L) {
    return(greeks_empty)
  }

  display_surface <- ea_filter_date_range(all_surface, filters$date_range)
  if (nrow(display_surface) == 0L) {
    return(utils::modifyList(greeks_empty, list(
      available_markets = unique(all_surface$market),
      notes = c("Options charts use the selected display range. The current display range contains no options observations.")
    )))
  }

  available_markets <- unique(display_surface$market)
  window_info <- ea_resolve_analysis_windows(display_surface$date, all_surface$date, history_context)
  display_window <- window_info$display_window
  context_window <- window_info$context_window
  latest_surface_date <- max(display_surface$date, na.rm = TRUE)
  context_surface <- all_surface %>%
    dplyr::filter(.data$date >= context_window[[1]], .data$date <= context_window[[2]])
  snapshot_surface <- display_surface %>%
    dplyr::filter(.data$date == latest_surface_date)

  option_is_call <- if ("option_type" %in% names(snapshot_surface)) {
    stringr::str_to_upper(snapshot_surface$option_type) == "C"
  } else {
    rep(TRUE, nrow(snapshot_surface))
  }

  snapshot_surface$risk_free <- purrr::map2_dbl(
    snapshot_surface$days_to_expiry,
    snapshot_surface$date,
    ~ ea_interpolate_risk_free(ust_curve, .x, .y)
  )

  greeks_result <- black76_greeks_vectorised(
    F = snapshot_surface$forward,
    K = snapshot_surface$strike,
    r = snapshot_surface$risk_free,
    T = snapshot_surface$T_years,
    sigma = snapshot_surface$iv,
    is_call = option_is_call
  )

  full_grid <- dplyr::bind_cols(
    snapshot_surface %>% dplyr::select(-dplyr::any_of(names(greeks_result))),
    greeks_result
  ) %>%
    dplyr::mutate(moneyness = round(.data$moneyness, 2))

  primary <- available_markets[[1]]

  cross_greeks_vanna <- full_grid %>%
    dplyr::filter(.data$market == primary) %>%
    dplyr::select(.data$curve_point_num, .data$moneyness, .data$vanna)

  cross_greeks_charm <- full_grid %>%
    dplyr::filter(.data$market == primary) %>%
    dplyr::select(.data$curve_point_num, .data$moneyness, .data$charm)

  term_greeks <- full_grid %>%
    dplyr::group_by(.data$market, .data$curve_point_num) %>%
    dplyr::slice_min(abs(.data$moneyness - 1.0), n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      curve_point = .data$contract_month,
      atm_iv = .data$iv,
      atm_gap = abs(.data$moneyness - 1.0)
    ) %>%
    dplyr::select(
      .data$market,
      .data$label,
      .data$curve_point,
      .data$curve_point_num,
      .data$moneyness,
      .data$atm_gap,
      .data$atm_iv,
      .data$gamma,
      .data$vega
    )

  prompt_num <- min(full_grid$curve_point_num, na.rm = TRUE)
  strike_profile <- full_grid %>%
    dplyr::filter(.data$market == primary, .data$curve_point_num == prompt_num) %>%
    dplyr::select(.data$moneyness, .data$strike, .data$speed, .data$zomma)

  greeks_concentration <- full_grid %>%
    dplyr::filter(abs(.data$moneyness - 1.0) < atm_tolerance) %>%
    dplyr::mutate(
      tenor_bucket = dplyr::case_when(
        .data$curve_point_num <= 1 ~ "Prompt",
        .data$curve_point_num <= 3 ~ "3M",
        .data$curve_point_num <= 6 ~ "6M",
        .data$curve_point_num <= 12 ~ "12M",
        TRUE ~ "Deferred"
      )
    ) %>%
    dplyr::group_by(.data$market, .data$label, .data$tenor_bucket) %>%
    dplyr::summarise(
      delta = mean(.data$delta, na.rm = TRUE),
      gamma = sum(.data$gamma, na.rm = TRUE),
      vega = sum(.data$vega, na.rm = TRUE),
      theta = sum(.data$theta, na.rm = TRUE),
      .groups = "drop"
    )

  skew_ratio <- full_grid %>%
    dplyr::filter(abs(.data$moneyness - 1.0) < atm_tolerance) %>%
    dplyr::group_by(.data$market, .data$curve_point_num) %>%
    dplyr::slice_min(abs(.data$moneyness - 1.0), n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::select(.data$market, .data$curve_point_num, atm_iv = .data$iv) %>%
    dplyr::left_join(
      full_grid %>%
        dplyr::filter(.data$option_type == "P", abs(.data$moneyness - 0.95) < 0.03) %>%
        dplyr::group_by(.data$market, .data$curve_point_num) %>%
        dplyr::slice_min(abs(.data$moneyness - 0.95), n = 1, with_ties = FALSE) %>%
        dplyr::ungroup() %>%
        dplyr::select(.data$market, .data$curve_point_num, otm_put_iv = .data$iv),
      by = c("market", "curve_point_num")
    ) %>%
    dplyr::mutate(skew_ratio = dplyr::if_else(.data$atm_iv > 0, .data$otm_put_iv / .data$atm_iv, NA_real_)) %>%
    dplyr::select(.data$market, .data$curve_point_num, .data$atm_iv, .data$otm_put_iv, .data$skew_ratio)

  build_atm_history <- function(surface_data) {
    if (nrow(surface_data) == 0L) {
      return(greeks_empty$atm_history)
    }

    atm_surface <- surface_data %>%
      dplyr::filter(abs(.data$moneyness - 1.0) < atm_tolerance) %>%
      dplyr::group_by(.data$date, .data$market, .data$curve_point_num) %>%
      dplyr::slice_min(abs(.data$moneyness - 1.0), n = 1, with_ties = FALSE) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(.data$market, .data$date, .data$curve_point_num)

    atm_surface$risk_free <- purrr::map2_dbl(
      atm_surface$days_to_expiry,
      atm_surface$date,
      ~ ea_interpolate_risk_free(ust_curve, .x, .y)
    )

    greeks_hist <- black76_greeks_vectorised(
      F = atm_surface$forward,
      K = atm_surface$strike,
      r = atm_surface$risk_free,
      T = atm_surface$T_years,
      sigma = atm_surface$iv,
      is_call = stringr::str_to_upper(atm_surface$option_type) == "C"
    )

    atm_surface <- dplyr::bind_cols(
      atm_surface %>% dplyr::select(-dplyr::any_of(names(greeks_hist))),
      greeks_hist
    )

    skew_hist <- surface_data %>%
      dplyr::filter(.data$option_type == "P", abs(.data$moneyness - 0.95) < 0.03) %>%
      dplyr::group_by(.data$date, .data$market, .data$curve_point_num) %>%
      dplyr::slice_min(abs(.data$moneyness - 0.95), n = 1, with_ties = FALSE) %>%
      dplyr::ungroup() %>%
      dplyr::transmute(.data$date, .data$market, .data$curve_point_num, otm_put_iv = .data$iv)

    atm_surface %>%
      dplyr::left_join(skew_hist, by = c("date", "market", "curve_point_num")) %>%
      dplyr::mutate(skew_ratio = dplyr::if_else(.data$iv > 0, .data$otm_put_iv / .data$iv, NA_real_)) %>%
      dplyr::transmute(
        .data$date,
        .data$market,
        .data$curve_point_num,
        atm_iv = .data$iv,
        .data$skew_ratio,
        .data$gamma,
        .data$vega
      )
  }

  atm_history <- build_atm_history(display_surface)
  context_history <- build_atm_history(context_surface)

  surface_context <- context_history %>%
    dplyr::group_by(.data$market) %>%
    dplyr::filter(.data$curve_point_num == min(.data$curve_point_num, na.rm = TRUE)) %>%
    dplyr::summarise(
      atm_iv = dplyr::last(.data$atm_iv),
      iv_mean = mean(.data$atm_iv, na.rm = TRUE),
      iv_sd = stats::sd(.data$atm_iv, na.rm = TRUE),
      iv_percentile = mean(.data$atm_iv <= dplyr::last(.data$atm_iv), na.rm = TRUE),
      skew_ratio = dplyr::last(.data$skew_ratio),
      skew_mean = mean(.data$skew_ratio, na.rm = TRUE),
      skew_sd = stats::sd(.data$skew_ratio, na.rm = TRUE),
      skew_percentile = mean(.data$skew_ratio <= dplyr::last(.data$skew_ratio), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      iv_zscore = dplyr::if_else(is.finite(.data$iv_sd) & .data$iv_sd > 0, (.data$atm_iv - .data$iv_mean) / .data$iv_sd, NA_real_),
      skew_zscore = dplyr::if_else(is.finite(.data$skew_sd) & .data$skew_sd > 0, (.data$skew_ratio - .data$skew_mean) / .data$skew_sd, NA_real_)
    ) %>%
    dplyr::select(.data$market, .data$atm_iv, .data$iv_percentile, .data$iv_zscore, .data$skew_ratio, .data$skew_percentile, .data$skew_zscore)

  primary_atm <- full_grid %>%
    dplyr::filter(.data$market == primary, abs(.data$moneyness - 1.0) < atm_tolerance) %>%
    dplyr::group_by(.data$curve_point_num) %>%
    dplyr::slice_min(abs(.data$moneyness - 1.0), n = 1, with_ties = FALSE) %>%
    dplyr::ungroup()

  pnl_grid <- {
    primary_atm_candidates <- full_grid %>%
      dplyr::filter(.data$market == primary, abs(.data$moneyness - 1.0) < atm_tolerance, .data$curve_point_num == min(.data$curve_point_num))

    primary_atm_row <- primary_atm_candidates %>%
      dplyr::filter(.data$option_type == "C") %>%
      dplyr::slice_min(abs(.data$moneyness - 1.0), n = 1, with_ties = FALSE)

    if (nrow(primary_atm_row) == 0L) {
      primary_atm_row <- primary_atm_candidates %>%
        dplyr::slice_min(abs(.data$moneyness - 1.0), n = 1, with_ties = FALSE)
    }

    if (nrow(primary_atm_row) == 0L) {
      tibble::tibble(spot_shock = numeric(), vol_shock = numeric(), pnl = numeric())
    } else {
      F0 <- primary_atm_row$forward[[1]]
      K <- primary_atm_row$strike[[1]]
      r <- primary_atm_row$risk_free[[1]]
      T_y <- primary_atm_row$T_years[[1]]
      sigma0 <- primary_atm_row$iv[[1]]
      is_call <- stringr::str_to_upper(primary_atm_row$option_type[[1]]) == "C"
      base_premium <- primary_atm_row$premium[[1]]
      if (is.null(base_premium) || is.na(base_premium)) {
        base_premium <- black76_greeks_vectorised(F0, K, r, T_y, sigma0, is_call)$premium
      }

      expand.grid(
        spot_shock = seq(-0.10, 0.10, by = 0.02),
        vol_shock = seq(-0.05, 0.05, by = 0.01)
      ) %>%
        tibble::as_tibble() %>%
        dplyr::mutate(
          F_new = F0 * (1 + .data$spot_shock),
          sigma_new = pmax(sigma0 + .data$vol_shock, 0.01),
          premium_new = black76_greeks_vectorised(
            .data$F_new,
            rep(K, dplyr::n()),
            rep(r, dplyr::n()),
            rep(T_y, dplyr::n()),
            .data$sigma_new,
            rep(is_call, dplyr::n())
          )$premium,
          pnl = .data$premium_new - base_premium
        ) %>%
        dplyr::select(.data$spot_shock, .data$vol_shock, .data$pnl)
    }
  }

  primary_atm_calls <- full_grid %>%
    dplyr::filter(.data$market == primary, .data$option_type == "C", abs(.data$moneyness - 1.0) < atm_tolerance) %>%
    dplyr::group_by(.data$curve_point_num) %>%
    dplyr::slice_min(abs(.data$moneyness - 1.0), n = 1, with_ties = FALSE) %>%
    dplyr::ungroup()

  primary_context <- surface_context %>% dplyr::filter(.data$market == primary)

  kpis <- tibble::tribble(
    ~title, ~value, ~delta, ~icon, ~status,
    "ATM Delta",
    if (nrow(primary_atm_calls) > 0L) scales::number(mean(primary_atm_calls$delta, na.rm = TRUE), accuracy = 0.001) else "N/A",
    "front call",
    "chart-line",
    "neutral",

    "Peak Gamma",
    if (nrow(primary_atm) > 0L) scales::scientific(max(primary_atm$gamma), digits = 3) else "N/A",
    "snapshot",
    "gauge-high",
    "neutral",

    "Front ATM IV",
    if (nrow(primary_context) > 0L && is.finite(primary_context$atm_iv[[1]])) scales::percent(primary_context$atm_iv[[1]], accuracy = 0.1) else "N/A",
    ea_history_context_label(history_context),
    "wave-square",
    "neutral",

    "Surface Points",
    scales::comma(nrow(full_grid)),
    as.character(latest_surface_date),
    "grid-4",
    "neutral"
  )

  list(
    available_markets = available_markets,
    history_context = history_context,
    history_context_label = ea_history_context_label(history_context),
    display_window = display_window,
    context_window = context_window,
    latest_surface_date = latest_surface_date,
    kpis = kpis,
    cross_greeks_vanna = cross_greeks_vanna,
    cross_greeks_charm = cross_greeks_charm,
    term_greeks = term_greeks,
    strike_profile = strike_profile,
    full_grid = full_grid,
    greeks_concentration = greeks_concentration,
    pnl_grid = pnl_grid,
    skew_ratio = skew_ratio,
    atm_history = atm_history,
    surface_context = surface_context,
    notes = c(
      "Surface heatmaps, smiles, and scenario grids use the latest options snapshot inside the selected display range.",
      paste0("ATM IV and skew context use the ", ea_history_context_label(history_context), " history window ending on that snapshot date."),
      "Greeks are recomputed with Black-76 using the matching UST curve date for each observation."
    ),
    assumptions = c(
      "Risk-free rates are interpolated linearly from the UST curve using the matching options observation date.",
      "ATM rows use the nearest listed strike to moneyness 1.00 within a small tolerance band.",
      "Options history is only available for the packaged options date range present in the local snapshot."
    )
  )
}
