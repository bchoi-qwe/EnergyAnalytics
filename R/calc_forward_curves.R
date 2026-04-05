# ---- Term Structure Calculation Layer ----
# Produces forward curve analytics from the canonical snapshot.

ea_calc_forward_curves <- function(filters) {
  catalog <- ea_market_catalog()
  curves <- ea_load_filtered_curves(filters)
  markets <- unique(curves$market)
  labels <- stats::setNames(catalog$label, catalog$market)
  latest_date <- max(curves$date, na.rm = TRUE)
  primary <- markets[1]

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

  # --- calendar_spread_strip: full M(n)-M(n+1) with z-scores ---
  calendar_spread_strip <- tryCatch({
    primary_wide <- curves |>
      dplyr::filter(.data$market == primary) |>
      ea_curve_to_wide()

    cp_cols <- setdiff(names(primary_wide), c("date", "market"))
    cp_cols <- sort(cp_cols)

    if (length(cp_cols) >= 2) {
      strip_rows <- purrr::map_dfr(seq_len(length(cp_cols) - 1L), function(i) {
        front_col <- cp_cols[i]
        back_col <- cp_cols[i + 1L]
        sp <- primary_wide[[front_col]] - primary_wide[[back_col]]
        sp_label <- paste0("M", as.integer(front_col), "-M", as.integer(back_col))

        ts_data <- tibble::tibble(date = primary_wide$date, spread_value = sp) |>
          dplyr::filter(!is.na(.data$spread_value)) |>
          dplyr::arrange(.data$date)

        if (nrow(ts_data) < 10) return(tibble::tibble())

        ts_data <- ts_data |>
          dplyr::mutate(
            rolling_mean = slider::slide_dbl(.data$spread_value, mean, .before = 251L, .complete = FALSE),
            rolling_sd = slider::slide_dbl(.data$spread_value, stats::sd, .before = 251L, .complete = FALSE)
          )

        latest_row <- utils::tail(ts_data, 1)
        current_spread <- latest_row$spread_value
        z <- if (!is.na(latest_row$rolling_sd) && latest_row$rolling_sd > 0) {
          (current_spread - latest_row$rolling_mean) / latest_row$rolling_sd
        } else {
          0
        }

        tibble::tibble(
          market = primary,
          spread_label = sp_label,
          front_tenor = as.integer(front_col),
          current_spread = current_spread,
          zscore = z
        )
      })
      strip_rows
    } else {
      tibble::tibble(market = character(), spread_label = character(), front_tenor = integer(),
                     current_spread = numeric(), zscore = numeric())
    }
  }, error = function(e) {
    tibble::tibble(market = character(), spread_label = character(), front_tenor = integer(),
                   current_spread = numeric(), zscore = numeric())
  })

  # --- prompt_spread_ts: M1-M2 with rolling percentile bands ---
  prompt_spread_ts <- tryCatch({
    primary_wide <- curves |>
      dplyr::filter(.data$market == primary) |>
      ea_curve_to_wide()

    if (all(c("01", "02") %in% names(primary_wide))) {
      ts_data <- tibble::tibble(
        date = primary_wide$date,
        market = primary,
        spread = primary_wide[["01"]] - primary_wide[["02"]]
      ) |>
        dplyr::filter(!is.na(.data$spread)) |>
        dplyr::arrange(.data$date)

      if (nrow(ts_data) >= 20) {
        ts_data <- ts_data |>
          dplyr::mutate(
            p5 = slider::slide_dbl(.data$spread,
              ~ stats::quantile(.x, 0.05, na.rm = TRUE), .before = 251L, .complete = FALSE),
            p25 = slider::slide_dbl(.data$spread,
              ~ stats::quantile(.x, 0.25, na.rm = TRUE), .before = 251L, .complete = FALSE),
            p75 = slider::slide_dbl(.data$spread,
              ~ stats::quantile(.x, 0.75, na.rm = TRUE), .before = 251L, .complete = FALSE),
            p95 = slider::slide_dbl(.data$spread,
              ~ stats::quantile(.x, 0.95, na.rm = TRUE), .before = 251L, .complete = FALSE)
          )
      }
      ts_data
    } else {
      tibble::tibble(date = as.Date(character()), market = character(), spread = numeric())
    }
  }, error = function(e) {
    tibble::tibble(date = as.Date(character()), market = character(), spread = numeric())
  })

  # --- roll_yield: annualized roll yield by tenor pair ---
  roll_yield <- tryCatch({
    primary_snap <- curve_snapshot |>
      dplyr::filter(.data$market == primary) |>
      dplyr::arrange(.data$curve_point_num)

    if (nrow(primary_snap) >= 2) {
      purrr::map_dfr(seq_len(nrow(primary_snap) - 1L), function(i) {
        fp <- primary_snap$price[i]
        bp <- primary_snap$price[i + 1L]
        tibble::tibble(
          market = primary,
          tenor_pair = paste0("M", primary_snap$curve_point_num[i], "-M", primary_snap$curve_point_num[i + 1L]),
          front_tenor = primary_snap$curve_point_num[i],
          front_price = fp,
          back_price = bp,
          spread = fp - bp,
          roll_yield_ann = (fp - bp) / fp * 12
        )
      })
    } else {
      tibble::tibble(market = character(), tenor_pair = character(), front_tenor = integer(),
                     front_price = numeric(), back_price = numeric(), spread = numeric(),
                     roll_yield_ann = numeric())
    }
  }, error = function(e) {
    tibble::tibble(market = character(), tenor_pair = character(), front_tenor = integer(),
                   front_price = numeric(), back_price = numeric(), spread = numeric(),
                   roll_yield_ann = numeric())
  })

  # --- curve_pca: PCA on log returns of each tenor ---
  curve_pca <- tryCatch({
    primary_long <- curves |>
      dplyr::filter(.data$market == primary, .data$value > 0)

    lr <- primary_long |>
      dplyr::arrange(.data$curve_point, .data$date) |>
      dplyr::group_by(.data$curve_point) |>
      dplyr::mutate(log_return = log(.data$value / dplyr::lag(.data$value))) |>
      dplyr::ungroup() |>
      dplyr::filter(!is.na(.data$log_return))

    ret_wide <- lr |>
      dplyr::select("date", "curve_point", "log_return") |>
      tidyr::pivot_wider(names_from = "curve_point", values_from = "log_return") |>
      dplyr::arrange(.data$date)

    tenor_cols <- setdiff(names(ret_wide), "date")
    mat <- as.matrix(ret_wide[, tenor_cols])
    mat <- mat[stats::complete.cases(mat), , drop = FALSE]

    if (nrow(mat) >= 20 && ncol(mat) >= 3) {
      pca_fit <- stats::prcomp(mat, center = TRUE, scale. = TRUE)
      n_pcs <- min(3, ncol(pca_fit$rotation))
      loadings <- tibble::tibble(
        tenor = tenor_cols,
        PC1 = pca_fit$rotation[, 1],
        PC2 = if (n_pcs >= 2) pca_fit$rotation[, 2] else NA_real_,
        PC3 = if (n_pcs >= 3) pca_fit$rotation[, 3] else NA_real_
      )
      var_exp <- summary(pca_fit)$importance[2, seq_len(n_pcs)]
      var_explained <- tibble::tibble(
        component = paste0("PC", seq_len(n_pcs)),
        proportion = as.numeric(var_exp)
      )
      list(loadings = loadings, var_explained = var_explained)
    } else {
      list(
        loadings = tibble::tibble(tenor = character(), PC1 = numeric(), PC2 = numeric(), PC3 = numeric()),
        var_explained = tibble::tibble(component = character(), proportion = numeric())
      )
    }
  }, error = function(e) {
    list(
      loadings = tibble::tibble(tenor = character(), PC1 = numeric(), PC2 = numeric(), PC3 = numeric()),
      var_explained = tibble::tibble(component = character(), proportion = numeric())
    )
  })

  # --- treasury_overlay: commodity spread vs Treasury 2s10s ---
  treasury_overlay <- tryCatch({
    ust <- ea_load_dataset("ust_curve_long")

    # Get 2s10s slope
    ust_2s10s <- ust |>
      dplyr::filter(.data$tenor %in% c("DGS2", "DGS10", "2", "10", "2Y", "10Y")) |>
      dplyr::select("date", "tenor", "value") |>
      tidyr::pivot_wider(names_from = "tenor", values_from = "value")

    tenor_cols_ust <- setdiff(names(ust_2s10s), "date")
    short_col <- tenor_cols_ust[grepl("2", tenor_cols_ust)][1]
    long_col <- tenor_cols_ust[grepl("10", tenor_cols_ust)][1]

    if (!is.na(short_col) && !is.na(long_col)) {
      ust_slope <- tibble::tibble(
        date = ust_2s10s$date,
        slope_2s10s = ust_2s10s[[long_col]] - ust_2s10s[[short_col]]
      ) |> dplyr::filter(!is.na(.data$slope_2s10s))

      # Get commodity M1-M12 spread for primary
      primary_wide <- curves |>
        dplyr::filter(.data$market == primary) |>
        ea_curve_to_wide()

      front_col <- "01"
      back_col <- if ("12" %in% names(primary_wide)) "12" else utils::tail(setdiff(sort(names(primary_wide)), c("date", "market")), 1)

      if (length(back_col) > 0 && all(c(front_col, back_col) %in% names(primary_wide))) {
        cmdty_sp <- tibble::tibble(
          date = primary_wide$date,
          cmdty_spread = primary_wide[[front_col]] - primary_wide[[back_col]]
        ) |> dplyr::filter(!is.na(.data$cmdty_spread))

        dplyr::inner_join(cmdty_sp, ust_slope, by = "date") |>
          dplyr::arrange(.data$date)
      } else {
        tibble::tibble(date = as.Date(character()), cmdty_spread = numeric(), slope_2s10s = numeric())
      }
    } else {
      tibble::tibble(date = as.Date(character()), cmdty_spread = numeric(), slope_2s10s = numeric())
    }
  }, error = function(e) {
    tibble::tibble(date = as.Date(character()), cmdty_spread = numeric(), slope_2s10s = numeric())
  })

  # --- structure_summary: enhanced curve shape metrics ---
  structure_summary <- tryCatch({
    curve_snapshot |>
      dplyr::group_by(.data$market) |>
      dplyr::summarise(
        m1_price = dplyr::first(.data$price),
        m1_m2 = {
          p <- .data$price
          cpn <- .data$curve_point_num
          m1_val <- p[cpn == min(cpn)][1]
          m2_val <- p[cpn == (min(cpn) + 1L)][1]
          if (!is.na(m2_val)) m1_val - m2_val else NA_real_
        },
        m1_m6 = {
          p <- .data$price
          cpn <- .data$curve_point_num
          m1_val <- p[cpn == min(cpn)][1]
          m6_val <- p[cpn == (min(cpn) + 5L)][1]
          if (!is.na(m6_val)) m1_val - m6_val else NA_real_
        },
        m1_m12 = {
          p <- .data$price
          cpn <- .data$curve_point_num
          m1_val <- p[cpn == min(cpn)][1]
          m12_val <- p[cpn == (min(cpn) + 11L)][1]
          if (!is.na(m12_val)) m1_val - m12_val else NA_real_
        },
        slope = dplyr::last(.data$price) - dplyr::first(.data$price),
        curvature = {
          mid <- .data$price[ceiling(dplyr::n() / 2)]
          avg_ends <- (dplyr::first(.data$price) + dplyr::last(.data$price)) / 2
          mid - avg_ends
        },
        .groups = "drop"
      ) |>
      dplyr::mutate(
        regime = dplyr::case_when(
          .data$slope < -3 ~ "Steep Backwardation",
          .data$slope < -0.01 & .data$curvature >= 0 ~ "Backwardation",
          .data$slope > 5 ~ "Super Contango",
          .data$slope > 0.01 & .data$curvature > 0.5 ~ "Humped Contango",
          .data$slope > 0.01 ~ "Contango",
          .data$curvature < -0.5 ~ "Inverted Belly",
          TRUE ~ "Flat"
        )
      )
  }, error = function(e) {
    tibble::tibble(market = character(), m1_price = numeric(), m1_m2 = numeric(),
                   m1_m6 = numeric(), m1_m12 = numeric(), slope = numeric(),
                   curvature = numeric(), regime = character())
  })

  # Add m1_m12_percentile and roll_yield_ann to structure_summary
  structure_summary <- tryCatch({
    structure_summary |>
      dplyr::rowwise() |>
      dplyr::mutate(
        m1_m12_percentile = {
          mkt_sp <- calendar_spreads |>
            dplyr::filter(.data$market == .data$market, .data$spread_label == "M1-M12")
          if (nrow(mkt_sp) > 10 && !is.na(.data$m1_m12)) {
            mean(mkt_sp$value <= .data$m1_m12, na.rm = TRUE)
          } else {
            NA_real_
          }
        },
        roll_yield_ann = {
          if (!is.na(.data$m1_price) && .data$m1_price != 0 && !is.na(.data$m1_m2)) {
            .data$m1_m2 / .data$m1_price * 12
          } else {
            NA_real_
          }
        }
      ) |>
      dplyr::ungroup()
  }, error = function(e) {
    structure_summary |>
      dplyr::mutate(m1_m12_percentile = NA_real_, roll_yield_ann = NA_real_)
  })

  # --- kpis ---
  kpis <- tryCatch({
    primary_summary <- structure_summary |>
      dplyr::filter(.data$market == primary)

    if (nrow(primary_summary) == 0) {
      return(tibble::tibble(title = character(), value = character(),
                            delta = character(), status = character()))
    }

    ps <- primary_summary[1, ]
    m1_price <- ps$m1_price
    m1_m2 <- ps$m1_m2
    m1_m12 <- ps$m1_m12
    regime_val <- ps$regime
    pctile <- ps$m1_m12_percentile
    ry <- ps$roll_yield_ann

    tibble::tibble(
      title = c("M1 Price", "Prompt Spread", "Front-Back", "Regime", "Curve Percentile", "Roll Yield"),
      value = c(
        scales::dollar(m1_price, accuracy = 0.01),
        scales::dollar(m1_m2, accuracy = 0.01),
        scales::dollar(m1_m12, accuracy = 0.01),
        as.character(regime_val),
        if (!is.na(pctile)) scales::percent(pctile, accuracy = 1) else "N/A",
        if (!is.na(ry)) scales::percent(ry, accuracy = 0.1) else "N/A"
      ),
      delta = c(
        "",
        if (!is.na(m1_m2)) paste0(ifelse(m1_m2 > 0, "+", ""), scales::number(m1_m2, accuracy = 0.01)) else "",
        if (!is.na(m1_m12)) paste0(ifelse(m1_m12 > 0, "+", ""), scales::number(m1_m12, accuracy = 0.01)) else "",
        "",
        if (!is.na(pctile)) paste0(scales::ordinal(round(pctile * 100)), " pctile") else "",
        if (!is.na(ry)) paste0(ifelse(ry > 0, "+", ""), scales::percent(ry, accuracy = 0.1)) else ""
      ),
      status = c(
        "neutral",
        if (!is.na(m1_m2) && m1_m2 > 0) "positive" else if (!is.na(m1_m2) && m1_m2 < -1) "warning" else "neutral",
        if (!is.na(m1_m12) && m1_m12 > 0) "positive" else if (!is.na(m1_m12) && m1_m12 < -1) "warning" else "neutral",
        if (grepl("Backwardation", regime_val)) "positive" else if (grepl("Contango", regime_val)) "warning" else "neutral",
        if (!is.na(pctile) && pctile > 0.7) "positive" else if (!is.na(pctile) && pctile < 0.3) "warning" else "neutral",
        if (!is.na(ry) && ry > 0) "positive" else if (!is.na(ry) && ry < 0) "warning" else "neutral"
      )
    )
  }, error = function(e) {
    tibble::tibble(title = character(), value = character(),
                   delta = character(), status = character())
  })

  # --- notes and assumptions ---
  notes <- c(
    "Forward curves sourced from RTL::dflong canonical snapshot.",
    "Calendar spreads computed as front minus back contract.",
    "Z-scores use trailing 252 business day window.",
    "PCA computed on log returns with centering and scaling.",
    "Roll yield annualized as (front - back) / front * 12."
  )

  assumptions <- c(
    "Curve points represent monthly contract expirations.",
    "Percentile bands use trailing 252-day rolling windows.",
    "Treasury 2s10s slope computed from FRED DGS2 and DGS10 series.",
    "Regime classification based on slope and curvature thresholds."
  )

  list(
    curve_snapshot = curve_snapshot,
    curve_history = curve_history,
    curve_change_heatmap = primary_recent,
    calendar_spreads = calendar_spreads,
    calendar_spread_strip = calendar_spread_strip,
    prompt_spread_ts = prompt_spread_ts,
    roll_yield = roll_yield,
    curve_pca = curve_pca,
    treasury_overlay = treasury_overlay,
    structure_summary = structure_summary,
    kpis = kpis,
    notes = notes,
    assumptions = assumptions
  )
}
