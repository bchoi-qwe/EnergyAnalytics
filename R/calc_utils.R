# ---- Shared Calculation Utilities ----
# Common helpers used by all calc_*.R modules.

ea_latest_snapshot_date <- function() {
  curves <- ea_load_dataset("commodity_curve_long")
  max(curves$date, na.rm = TRUE)
}

ea_load_filtered_curves <- function(filters) {
  curves <- ea_load_dataset("commodity_curve_long")
  markets <- ea_coalesce(filters$commodities, c("CL", "BRN", "RB"))
  expiry_lo <- ea_coalesce(filters$expiry_range[1], 1L)
  expiry_hi <- ea_coalesce(filters$expiry_range[2], 12L)

  out <- curves |>
    dplyr::filter(
      .data$market %in% markets,
      .data$curve_point_num >= expiry_lo,
      .data$curve_point_num <= expiry_hi
    )

  if (!is.null(filters$date_range) && length(filters$date_range) == 2L) {
    out <- out |>
      dplyr::filter(
        .data$date >= as.Date(filters$date_range[1]),
        .data$date <= as.Date(filters$date_range[2])
      )
  }

  out |> dplyr::arrange(.data$market, .data$curve_point_num, .data$date)
}

ea_log_returns <- function(curves_long) {
  curves_long |>
    dplyr::arrange(.data$market, .data$curve_point, .data$date) |>
    dplyr::group_by(.data$market, .data$curve_point) |>
    dplyr::mutate(log_return = suppressWarnings(log(.data$value / dplyr::lag(.data$value)))) |>
    dplyr::ungroup()
}

ea_curve_to_wide <- function(curves_long) {
  curves_long |>
    dplyr::select("date", "market", "curve_point", "value") |>
    tidyr::pivot_wider(names_from = "curve_point", values_from = "value")
}
