# ---- Volatility Calculation Layer ----
# Realized vol, GARCH, vol surface, and implied vs realized analytics.

ea_calc_volatility <- function(filters) {
  catalog <- ea_market_catalog()
  curves <- ea_load_filtered_curves(filters)
  markets <- unique(curves$market)
  primary <- markets[1]
  labels <- stats::setNames(catalog$label, catalog$market)

  # Front contract returns for realized vol
  front <- curves |>
    dplyr::filter(.data$curve_point_num == 1) |>
    dplyr::arrange(.data$market, .data$date) |>
    dplyr::group_by(.data$market) |>
    dplyr::mutate(log_return = log(.data$value / dplyr::lag(.data$value))) |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(.data$log_return))

  # --- realized_vol_timeseries: rolling vol at 20d, 60d, 120d ---
  windows <- c("20d" = 20L, "60d" = 60L, "120d" = 120L)
  annualize <- sqrt(252)

  realized_vol_timeseries <- purrr::map_dfr(names(windows), function(w) {
    n <- windows[[w]]
    front |>
      dplyr::group_by(.data$market) |>
      dplyr::mutate(
        realized_vol = slider::slide_dbl(
          .data$log_return, sd, .before = n - 1L, .complete = TRUE
        ) * annualize
      ) |>
      dplyr::ungroup() |>
      dplyr::filter(!is.na(.data$realized_vol)) |>
      dplyr::transmute(
        date = .data$date, market = .data$market,
        window = w, realized_vol = .data$realized_vol
      )
  })

  # --- garch_vol: GARCH(1,1) for primary market ---
  primary_front <- front |>
    dplyr::filter(.data$market == primary) |>
    dplyr::select(.data$date, value = .data$log_return)

  garch_vol <- tryCatch({
    garch_xts <- RTL::garch(x = primary_front, out = "data")
    tibble::tibble(
      date = as.Date(zoo::index(garch_xts)),
      returns = as.numeric(zoo::coredata(garch_xts[, "returns"])),
      garch_vol = as.numeric(zoo::coredata(garch_xts[, "garch"]))
    )
  }, error = function(e) {
    tibble::tibble(date = as.Date(character()), returns = numeric(), garch_vol = numeric())
  })

  # --- vol_term_structure: realized vol by tenor ---
  vol_term_structure <- curves |>
    dplyr::arrange(.data$market, .data$curve_point_num, .data$date) |>
    dplyr::group_by(.data$market, .data$curve_point_num) |>
    dplyr::mutate(log_return = log(.data$value / dplyr::lag(.data$value))) |>
    dplyr::filter(!is.na(.data$log_return)) |>
    dplyr::summarise(
      realized_vol = sd(.data$log_return, na.rm = TRUE) * annualize,
      .groups = "drop"
    ) |>
    dplyr::rename(tenor = .data$curve_point_num)

  # --- vol_surface_grid: parametric IV surface ---
  latest_date <- max(curves$date, na.rm = TRUE)
  forwards_df <- curves |> dplyr::filter(.data$date == latest_date)

  vol_surface_grid <- ea_generate_vol_surface(markets, forwards_df, catalog)

  # --- vol_skew_snapshot: front tenor IV smile ---
  front_tenor <- min(vol_surface_grid$curve_point_num, na.rm = TRUE)
  vol_skew_snapshot <- vol_surface_grid |>
    dplyr::filter(.data$curve_point_num == front_tenor) |>
    dplyr::select(.data$market, .data$moneyness, .data$iv)

  # --- realized_vs_implied: 60d realized vs ATM implied ---
  rv_60 <- realized_vol_timeseries |>
    dplyr::filter(.data$window == "60d") |>
    dplyr::select(.data$date, .data$market, realized = .data$realized_vol)

  atm_iv <- vol_surface_grid |>
    dplyr::filter(abs(.data$moneyness - 1.0) < 0.01, .data$curve_point_num == front_tenor) |>
    dplyr::select(.data$market, implied = .data$iv)

  realized_vs_implied <- rv_60 |>
    dplyr::left_join(atm_iv, by = "market")

  list(
    realized_vol_timeseries = realized_vol_timeseries,
    garch_vol = garch_vol,
    vol_term_structure = vol_term_structure,
    vol_surface_grid = vol_surface_grid,
    vol_skew_snapshot = vol_skew_snapshot,
    realized_vs_implied = realized_vs_implied
  )
}
