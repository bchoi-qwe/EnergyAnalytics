# data-raw/06_build_options_base.R
# Pipeline to process raw options chains, calculate IV/Greeks, and save to feather.

source("data-raw/00_setup.R")
# Reload with internal exports for pipeline use
pkgload::load_all(export_all = TRUE, quiet = TRUE)

# 1. Load Data
options_files <- list.files("data-raw/options_chains", pattern = "\\.csv$", full.names = TRUE)
raw_options <- purrr::map_dfr(options_files, readr::read_csv, show_col_types = FALSE)

futures <- arrow::read_feather("inst/extdata/latest/commodity_curve_long.feather") |>
  dplyr::filter(curve_point == "01") |>
  dplyr::select(date, market, forward = value)

ust_curve <- arrow::read_feather("inst/extdata/latest/ust_curve_long.feather")
expiry_meta <- arrow::read_feather("inst/extdata/latest/contract_expiry_metadata.feather")

# 2. Pre-process
options_base <- raw_options |>
  dplyr::mutate(
    obs_date = as.Date(obs_date),
    contract_year = as.integer(substr(contract_month, 1, 4)),
    contract_month_num = as.integer(substr(contract_month, 6, 7))
  ) |>
  dplyr::inner_join(futures, by = c("obs_date" = "date", "market")) |>
  dplyr::left_join(
    expiry_meta |> dplyr::select(market, contract_year, contract_month_num = contract_month, last_trade),
    by = c("market", "contract_year", "contract_month_num")
  ) |>
  dplyr::mutate(
    dte = pmax(as.numeric(difftime(last_trade, obs_date, units = "days")), 1),
    T_years = dte / 365.25,
    risk_free = ea_interpolate_risk_free(ust_curve, dte)
  )

# 3. Calculate IV (Newton-Raphson)
# Simple Black-76 IV solver for the pipeline
calc_iv <- function(F, K, r, T, premium, is_call) {
  sig <- rep(0.3, length(F)) # initial guess
  for (i in 1:15) {
    g <- black76_greeks_vectorised(F, K, r, T, sig, is_call)
    diff <- g$premium - premium
    # Handle NAs and select indices where tolerance isn't met
    idx <- !is.na(diff) & abs(diff) > 1e-6
    if (!any(idx)) break
    
    # Avoid division by zero or very small vega
    update_idx <- idx & !is.na(g$vega) & g$vega > 1e-8
    if (!any(update_idx)) break
    
    sig[update_idx] <- sig[update_idx] - (diff[update_idx] / g$vega[update_idx])
    sig <- pmax(pmin(sig, 5.0), 0.001) # Keep sig in reasonable bounds
  }
  sig
}

options_base$iv <- calc_iv(
  options_base$forward, options_base$strike, options_base$risk_free,
  options_base$T_years, options_base$premium, options_base$type == "C"
)

# 4. Calculate All Greeks
greeks <- black76_greeks_vectorised(
  options_base$forward, options_base$strike, options_base$risk_free,
  options_base$T_years, options_base$iv, options_base$type == "C"
)

# Align column names with the Unified Cube Spec
final_surface <- options_base |>
  dplyr::mutate(
    implied_volatility = iv,
    delta = greeks$delta,
    gamma = greeks$gamma,
    vega = greeks$vega,
    theta = greeks$theta
  ) |>
  dplyr::select(
    date = obs_date,
    market,
    contract_month,
    days_to_expiry = dte,
    strike,
    option_type = type,
    underlying_price = forward,
    implied_volatility,
    delta,
    gamma,
    vega,
    theta
  )

# 5. Save
arrow::write_feather(
  final_surface,
  "inst/extdata/latest/options_surface_long.feather",
  compression = "zstd"
)

message("Options surface build complete: ", nrow(final_surface), " rows saved.")
