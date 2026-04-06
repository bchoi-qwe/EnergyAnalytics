mod_volatility_ui <- function(id) {
  shiny::uiOutput(shiny::NS(id, "page"))
}

mod_volatility_server <- function(id, filters, data_timestamp) {
  shiny::moduleServer(id, function(input, output, session) {
    page_data <- shiny::reactive(ea_calc_volatility(filters()))

    mod_kpi_strip_server("kpi_strip", kpis = shiny::reactive(page_data()$kpis))
    mod_footer_notes_server(
      "footer_notes",
      notes = shiny::reactive(list(
        notes = page_data()$notes,
        assumptions = page_data()$assumptions,
        timestamp = data_timestamp()
      )),
      title = "Vol surface notes"
    )

    output$page <- shiny::renderUI({
      ns <- session$ns
      current_filters <- filters()

      if (length(current_filters$commodities) == 0L) {
        return(htmltools::tagList(
          ea_selected_filters_ribbon(current_filters, data_timestamp()),
          ea_empty_state_card(
            title = "Select products to populate vol views",
            body = "Vol Surface tracks realized vol cone, regime classification, ATM vol history, and implied vs realized spread across the selected products.",
            hint = "All vol views key off the same product selection and lookback."
          ),
          mod_footer_notes_ui(ns("footer_notes"))
        ))
      }

      htmltools::tagList(
        ea_selected_filters_ribbon(current_filters, data_timestamp()),
        mod_kpi_strip_ui(ns("kpi_strip")),
        # Row 1: Vol Cone (8) | Vol Regime Table (4)
        bslib::layout_columns(
          col_widths = c(8, 4),
          ea_plotly_card(
            title = "Vol Cone",
            subtitle = "Realized vol percentile fan by horizon.",
            output_id = ns("vol_cone"),
            height = "260px"
          ),
          ea_table_card(
            title = "Vol Regime Summary",
            subtitle = "Current regime classification per commodity.",
            output_id = ns("vol_regime_table")
          )
        ),
        # Row 2: ATM Vol History (6) | Vol-of-Vol Monitor (6)
        bslib::layout_columns(
          col_widths = c(6, 6),
          ea_plotly_card(
            title = "ATM Vol History",
            subtitle = "20d RV, 60d RV, GARCH, and ATM implied for primary market.",
            output_id = ns("atm_vol_history"),
            height = "250px"
          ),
          ea_plotly_card(
            title = "Vol-of-Vol Monitor",
            subtitle = "Rolling 60-day SD of 20d realized vol.",
            output_id = ns("vol_of_vol_chart"),
            height = "250px"
          )
        ),
        # Row 3: Realized Vol Surface heatmap (8) | Implied Vol Term Structure (4)
        bslib::layout_columns(
          col_widths = c(8, 4),
          ea_plotly_card(
            title = "Realized Vol Surface",
            subtitle = "Realized vol by market and tenor.",
            output_id = ns("rv_surface"),
            height = "260px"
          ),
          ea_plotly_card(
            title = "Implied Vol Term Structure",
            subtitle = "ATM implied vol by tenor per commodity.",
            output_id = ns("iv_term"),
            height = "260px"
          )
        ),
        # Row 4: Skew Chart (6) | Cross-Asset Vol Box (6)
        bslib::layout_columns(
          col_widths = c(6, 6),
          ea_plotly_card(
            title = "Skew / Smile",
            subtitle = "Front-month implied vol smile per commodity.",
            output_id = ns("vol_skew"),
            height = "250px"
          ),
          ea_plotly_card(
            title = "Cross-Asset Vol Comparison",
            subtitle = "Vol distribution box plot with current level marker.",
            output_id = ns("cross_asset_vol"),
            height = "250px"
          )
        ),
        # Row 5: IV vs RV Spread History (12)
        bslib::layout_columns(
          col_widths = c(12),
          ea_plotly_card(
            title = "IV vs RV Spread History",
            subtitle = "ATM implied vs 60d realized. Teal = options rich (IV > RV); red = options cheap (IV < RV).",
            output_id = ns("iv_rv_spread_chart"),
            height = "240px"
          )
        ),
        mod_footer_notes_ui(ns("footer_notes"))
      )
    })

    # --- Row 1 Left: Vol Cone ---
    output$vol_cone <- plotly::renderPlotly({
      vc <- page_data()$vol_cone
      palette <- ea_market_palette()
      fig <- plotly::plot_ly()

      if (nrow(vc) == 0) {
        return(ea_plotly_layout(fig, x_title = "Horizon (Days)", y_title = "Annualized Vol"))
      }

      for (mkt in unique(vc$market)) {
        df <- vc[vc$market == mkt, , drop = FALSE]
        col <- if (!is.null(palette[[mkt]])) unname(palette[[mkt]]) else "#4da3a3"

        # p10-p90 outer band
        fig <- fig |>
          plotly::add_ribbons(
            data = df,
            x = ~horizon,
            ymin = ~p10,
            ymax = ~p90,
            name = paste(mkt, "p10-p90"),
            fillcolor = paste0(col, "22"),
            line = list(color = "transparent"),
            showlegend = FALSE,
            hoverinfo = "skip"
          ) |>
          # p25-p75 inner band
          plotly::add_ribbons(
            data = df,
            x = ~horizon,
            ymin = ~p25,
            ymax = ~p75,
            name = paste(mkt, "p25-p75"),
            fillcolor = paste0(col, "55"),
            line = list(color = "transparent"),
            showlegend = FALSE,
            hoverinfo = "skip"
          ) |>
          # p50 median line
          plotly::add_lines(
            data = df,
            x = ~horizon,
            y = ~p50,
            name = paste(mkt, "median"),
            line = list(color = col, width = 1.5, dash = "dot"),
            showlegend = FALSE,
            hoverinfo = "skip"
          ) |>
          # Current vol marker
          plotly::add_markers(
            data = df,
            x = ~horizon,
            y = ~current_vol,
            name = mkt,
            marker = list(color = col, size = 8, symbol = "circle"),
            hovertemplate = paste0(mkt, " %{x}d<br>Current: %{y:.1%}<extra></extra>")
          )
      }

      ea_plotly_layout(fig, x_title = "Horizon (Days)", y_title = "Annualized Vol")
    })

    # --- Row 1 Right: Vol Regime Table ---
    output$vol_regime_table <- reactable::renderReactable({
      cav <- page_data()$cross_asset_vol
      vr <- page_data()$vol_regime

      tbl <- if (nrow(cav) > 0 && nrow(vr) > 0) {
        dplyr::left_join(vr, cav[, c("market", "current_vol")], by = "market")
      } else if (nrow(vr) > 0) {
        vr
      } else {
        tibble::tibble(market = character(), regime = character(), vol_percentile = numeric(), current_vol = numeric())
      }

      reactable::reactable(
        tbl,
        theme = ea_reactable_theme(),
        columns = list(
          market = reactable::colDef(name = "Market", minWidth = 70),
          regime = reactable::colDef(name = "Regime", minWidth = 140),
          vol_percentile = reactable::colDef(
            name = "Pctile",
            format = reactable::colFormat(percent = TRUE, digits = 0),
            minWidth = 70
          ),
          current_vol = reactable::colDef(
            name = "Current Vol",
            format = reactable::colFormat(percent = TRUE, digits = 1),
            minWidth = 90
          )
        ),
        compact = TRUE,
        highlight = TRUE
      )
    })

    # --- Row 2 Left: ATM Vol History ---
    output$atm_vol_history <- plotly::renderPlotly({
      rv_ts <- page_data()$realized_vol_timeseries
      garch <- page_data()$garch_vol
      atm_iv_val <- page_data()$vol_surface_grid
      primary_markets <- unique(rv_ts$market)
      primary <- if (length(primary_markets) > 0) primary_markets[1] else character(0)

      if (length(primary) == 0) {
        return(ea_plotly_layout(plotly::plot_ly(), x_title = NULL, y_title = "Annualized Vol"))
      }

      front_tenor <- if (nrow(atm_iv_val) > 0) min(atm_iv_val$curve_point_num, na.rm = TRUE) else 1L
      atm_iv_val_scalar <- atm_iv_val |>
        dplyr::filter(
          .data$market == primary,
          abs(.data$moneyness - 1.0) < 0.01,
          .data$curve_point_num == front_tenor
        ) |>
        dplyr::summarise(atm_iv = mean(.data$iv, na.rm = TRUE)) |>
        dplyr::pull(.data$atm_iv)
      if (length(atm_iv_val_scalar) == 0) atm_iv_val_scalar <- NA_real_

      rv20 <- rv_ts |> dplyr::filter(.data$market == primary, .data$window == "20d")
      rv60 <- rv_ts |> dplyr::filter(.data$market == primary, .data$window == "60d")
      garch_primary <- garch

      fig <- plotly::plot_ly()

      if (nrow(rv20) > 0) {
        fig <- fig |>
          plotly::add_lines(
            data = rv20,
            x = ~date,
            y = ~realized_vol,
            name = "20d RV",
            line = list(color = "#4da3a3", width = 1.5),
            hovertemplate = "%{x|%d %b %Y}<br>20d RV: %{y:.1%}<extra></extra>"
          )
      }

      if (nrow(rv60) > 0) {
        fig <- fig |>
          plotly::add_lines(
            data = rv60,
            x = ~date,
            y = ~realized_vol,
            name = "60d RV",
            line = list(color = "#4da3a3", width = 1.5, dash = "dash"),
            hovertemplate = "%{x|%d %b %Y}<br>60d RV: %{y:.1%}<extra></extra>"
          )
      }

      if (nrow(garch_primary) > 0) {
        fig <- fig |>
          plotly::add_lines(
            data = garch_primary,
            x = ~date,
            y = ~garch_vol,
            name = "GARCH",
            line = list(color = "#5a85c8", width = 1.5),
            hovertemplate = "%{x|%d %b %Y}<br>GARCH: %{y:.1%}<extra></extra>"
          )
      }

      if (!is.na(atm_iv_val_scalar) && nrow(rv60) > 0) {
        date_range <- range(rv60$date, na.rm = TRUE)
        fig <- fig |>
          plotly::add_lines(
            x = c(date_range[1], date_range[2]),
            y = c(atm_iv_val_scalar, atm_iv_val_scalar),
            name = "ATM IV",
            line = list(color = "#d2a157", width = 1.5, dash = "dot"),
            hovertemplate = "ATM IV: %{y:.1%}<extra></extra>"
          )
      }

      ea_plotly_layout(fig, x_title = NULL, y_title = "Annualized Vol")
    })

    # --- Row 2 Right: Vol-of-Vol ---
    output$vol_of_vol_chart <- plotly::renderPlotly({
      vov <- page_data()$vol_of_vol
      palette <- ea_market_palette()
      fig <- plotly::plot_ly()

      if (nrow(vov) == 0) {
        return(ea_plotly_layout(fig, x_title = NULL, y_title = "Vol of Vol"))
      }

      for (mkt in unique(vov$market)) {
        df <- vov[vov$market == mkt, , drop = FALSE]
        col <- if (!is.null(palette[[mkt]])) unname(palette[[mkt]]) else "#4da3a3"
        fig <- fig |>
          plotly::add_lines(
            data = df,
            x = ~date,
            y = ~vol_of_vol,
            name = mkt,
            line = list(color = col, width = 1.5),
            hovertemplate = "%{x|%d %b %Y}<br>VoV: %{y:.1%}<extra>%{fullData.name}</extra>"
          )
      }

      ea_plotly_layout(fig, x_title = NULL, y_title = "Vol of Vol")
    })

    # --- Row 3 Left: Realized Vol Surface Heatmap ---
    output$rv_surface <- plotly::renderPlotly({
      vts <- page_data()$vol_term_structure

      if (nrow(vts) == 0) {
        return(ea_plotly_layout(plotly::plot_ly(), x_title = "Market", y_title = "Tenor"))
      }

      vts_dedup <- dplyr::distinct(vts, .data$market, .data$tenor, .keep_all = TRUE)
      surface_matrix <- stats::xtabs(realized_vol ~ tenor + market, data = vts_dedup)

      fig <- plotly::plot_ly(
        x = colnames(surface_matrix),
        y = rownames(surface_matrix),
        z = unclass(surface_matrix),
        type = "heatmap",
        colors = c("#17202b", "#5a85c8", "#d2a157"),
        hovertemplate = "Market: %{x}<br>Tenor: %{y}<br>RV: %{z:.1%}<extra>realized vol</extra>"
      )

      ea_plotly_layout(fig, x_title = "Market", y_title = "Tenor", hovermode = "closest")
    })

    # --- Row 3 Right: Implied Vol Term Structure ---
    output$iv_term <- plotly::renderPlotly({
      vsg <- page_data()$vol_surface_grid
      palette <- ea_market_palette()
      fig <- plotly::plot_ly()

      atm <- vsg |> dplyr::filter(abs(.data$moneyness - 1.0) < 0.01)

      if (nrow(atm) == 0) {
        return(ea_plotly_layout(fig, x_title = "Contract Month", y_title = "ATM IV"))
      }

      for (mkt in unique(atm$market)) {
        df <- atm[atm$market == mkt, , drop = FALSE]
        col <- if (!is.null(palette[[mkt]])) unname(palette[[mkt]]) else "#4da3a3"
        fig <- fig |>
          plotly::add_lines(
            data = df,
            x = ~curve_point_num,
            y = ~iv,
            name = mkt,
            line = list(color = col, width = 2),
            hovertemplate = "Tenor M%{x}<br>ATM IV: %{y:.1%}<extra>%{fullData.name}</extra>"
          )
      }

      ea_plotly_layout(fig, x_title = "Contract Month", y_title = "ATM IV")
    })

    # --- Row 4 Left: Skew Chart ---
    output$vol_skew <- plotly::renderPlotly({
      skew <- page_data()$vol_skew_snapshot
      palette <- ea_market_palette()
      fig <- plotly::plot_ly()

      if (nrow(skew) == 0) {
        return(ea_plotly_layout(fig, x_title = "Moneyness", y_title = "Implied Vol", hovermode = "closest"))
      }

      for (mkt in unique(skew$market)) {
        df <- skew[skew$market == mkt, , drop = FALSE]
        col <- if (!is.null(palette[[mkt]])) unname(palette[[mkt]]) else "#4da3a3"
        fig <- fig |>
          plotly::add_lines(
            data = df,
            x = ~moneyness,
            y = ~iv,
            name = mkt,
            line = list(color = col, width = 2),
            hovertemplate = "Moneyness %{x:.0%}<br>IV: %{y:.1%}<extra>%{fullData.name}</extra>"
          )
      }

      ea_plotly_layout(fig, x_title = "Moneyness", y_title = "Implied Vol", hovermode = "closest")
    })

    # --- Row 4 Right: Cross-Asset Vol Box Plot ---
    output$cross_asset_vol <- plotly::renderPlotly({
      cav <- page_data()$cross_asset_vol
      palette <- ea_market_palette()
      fig <- plotly::plot_ly()

      if (nrow(cav) == 0) {
        return(ea_plotly_layout(fig, x_title = "Market", y_title = "Realized Vol", hovermode = "closest"))
      }

      for (mkt in unique(cav$market)) {
        df <- cav[cav$market == mkt, , drop = FALSE]
        col <- if (!is.null(palette[[mkt]])) unname(palette[[mkt]]) else "#4da3a3"
        fig <- fig |>
          plotly::add_trace(
            type = "box",
            name = mkt,
            x = mkt,
            lowerfence = df$min_vol,
            q1 = df$q1,
            median = df$median_vol,
            q3 = df$q3,
            upperfence = df$max_vol,
            marker = list(color = col),
            line = list(color = col),
            hoverinfo = "y+name"
          ) |>
          plotly::add_markers(
            x = mkt,
            y = df$current_vol,
            name = paste(mkt, "current"),
            marker = list(color = "#d2a157", size = 10, symbol = "diamond"),
            showlegend = FALSE,
            hovertemplate = paste0(mkt, " current: %{y:.1%}<extra></extra>")
          )
      }

      ea_plotly_layout(fig, x_title = "Market", y_title = "Realized Vol", hovermode = "closest")
    })

    # --- Row 5: IV vs RV Spread History ---
    output$iv_rv_spread_chart <- plotly::renderPlotly({
      ivr <- page_data()$iv_rv_spread
      fig <- plotly::plot_ly()

      if (nrow(ivr) == 0 || all(is.na(ivr$atm_iv)) || all(is.na(ivr$realized))) {
        return(ea_plotly_layout(fig, x_title = NULL, y_title = "Annualized Vol"))
      }

      ivr_clean <- ivr |> dplyr::filter(!is.na(.data$realized), !is.na(.data$atm_iv))

      if (nrow(ivr_clean) == 0) {
        return(ea_plotly_layout(fig, x_title = NULL, y_title = "Annualized Vol"))
      }

      # Rich ribbon: where iv > realized (teal fill)
      rich <- ivr_clean |> dplyr::mutate(
        ymin_rich = ifelse(.data$atm_iv >= .data$realized, .data$realized, .data$atm_iv),
        ymax_rich = ifelse(.data$atm_iv >= .data$realized, .data$atm_iv, .data$realized)
      )

      fig <- fig |>
        # Background ribbon - rich (IV > RV): teal
        plotly::add_ribbons(
          data = rich,
          x = ~date,
          ymin = ~ifelse(atm_iv >= realized, realized, atm_iv),
          ymax = ~ifelse(atm_iv >= realized, atm_iv, realized),
          name = "Options Rich",
          fillcolor = "rgba(77, 163, 163, 0.25)",
          line = list(color = "transparent"),
          showlegend = TRUE,
          hoverinfo = "skip"
        ) |>
        # Realized vol line
        plotly::add_lines(
          data = ivr_clean,
          x = ~date,
          y = ~realized,
          name = "60d Realized",
          line = list(color = "#4da3a3", width = 2),
          hovertemplate = "%{x|%d %b %Y}<br>60d RV: %{y:.1%}<extra></extra>"
        ) |>
        # ATM IV line
        plotly::add_lines(
          data = ivr_clean,
          x = ~date,
          y = ~atm_iv,
          name = "ATM Implied",
          line = list(color = "#d2a157", width = 2, dash = "dot"),
          hovertemplate = "%{x|%d %b %Y}<br>ATM IV: %{y:.1%}<extra></extra>"
        )

      ea_plotly_layout(fig, x_title = NULL, y_title = "Annualized Vol")
    })
  })
}
