# ---- Term Structure Calculation Layer ----
# Produces forward curve analytics from the canonical snapshot.

ea_calc_forward_curves <- function(filters) {
  catalog <- ea_market_catalog()
  curves <- ea_load_filtered_curves(filters)
  markets <- unique(curves$market)
  labels <- stats::setNames(catalog$label, catalog$market)
  latest_date <- max(curves$date, na.rm = TRUE)

  # --- curve_snapshot: current forward curve ---
  curve_snapshot <- curves |>
    dplyr::filter(.data$date == latest_date) |>
    dplyr::mutate(
      price = .data$value,
      label = labels[.data$market]
    ) |>
    dplyr::select("market", "curve_point", "curve_point_num", "price", "label")

  # --- curve_history: historical curve snapshots ---
  snapshot_offsets <- c("Latest" = 0, "1W Ago" = 7, "1M Ago" = 30, "3M Ago" = 90)
  primary <- markets[1]
  primary_curves <- curves |> dplyr::filter(.data$market == primary)
  available_dates <- sort(unique(primary_curves$date), decreasing = TRUE)

  curve_history <- purrr::map_dfr(names(snapshot_offsets), function(lbl) {
    target <- latest_date - snapshot_offsets[[lbl]]
    idx <- which.min(abs(as.numeric(available_dates - target)))
    snap_date <- available_dates[idx]

    primary_curves |>
      dplyr::filter(.data$date == snap_date) |>
      dplyr::transmute(
        snapshot_label = lbl,
        curve_point_num = .data$curve_point_num,
        price = .data$value
      )
  })

  # --- curve_change_heatmap: daily changes by tenor ---
  primary_recent <- primary_curves |>
    dplyr::filter(.data$date >= latest_date - 60) |>
    dplyr::arrange(.data$curve_point_num, .data$date) |>
    dplyr::group_by(.data$curve_point_num) |>
    dplyr::mutate(change = .data$value - dplyr::lag(.data$value)) |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(.data$change)) |>
    dplyr::select("date", "curve_point_num", "change")

  # --- calendar_spreads: M1-M2, M1-M3 timeseries ---
  calendar_spreads <- purrr::map_dfr(markets, function(mkt) {
    mkt_wide <- curves |>
      dplyr::filter(.data$market == mkt) |>
      ea_curve_to_wide()

    spread_pairs <- list(
      "M1-M2" = c("01", "02"),
      "M1-M3" = c("01", "03"),
      "M1-M6" = c("01", "06"),
      "M1-M12" = c("01", "12")
    )

    purrr::map_dfr(names(spread_pairs), function(sp_label) {
      cols <- spread_pairs[[sp_label]]
      if (!all(cols %in% names(mkt_wide))) return(tibble::tibble())

      tibble::tibble(
        date = mkt_wide$date,
        market = mkt,
        spread_label = sp_label,
        value = mkt_wide[[cols[1]]] - mkt_wide[[cols[2]]]
      ) |> dplyr::filter(!is.na(.data$value))
    })
  })

  # --- curve_shape_metrics ---
  curve_shape_metrics <- curve_snapshot |>
    dplyr::group_by(.data$market) |>
    dplyr::summarise(
      slope = dplyr::last(.data$price) - dplyr::first(.data$price),
      curvature = {
        mid <- .data$price[ceiling(dplyr::n() / 2)]
        avg_ends <- (dplyr::first(.data$price) + dplyr::last(.data$price)) / 2
        mid - avg_ends
      },
      .groups = "drop"
    ) |>
    dplyr::mutate(
      structure_label = dplyr::case_when(
        .data$slope > 0.01 ~ "Contango",
        .data$slope < -0.01 ~ "Backwardation",
        TRUE ~ "Flat"
      )
    )

  list(
    curve_snapshot = curve_snapshot,
    curve_history = curve_history,
    curve_change_heatmap = primary_recent,
    calendar_spreads = calendar_spreads,
    curve_shape_metrics = curve_shape_metrics
  )
}
