mod_fundamentals_ui <- function(id) {
  shiny::uiOutput(shiny::NS(id, "page"))
}

mod_fundamentals_server <- function(id, filters, data_timestamp) {
  shiny::moduleServer(id, function(input, output, session) {
    page_data <- shiny::reactive({ ea_calc_fundamentals(filters()) })

    mod_kpi_strip_server("kpi_strip", kpis = shiny::reactive(page_data()$kpis))
    mod_footer_notes_server(
      "footer_notes",
      notes = shiny::reactive(list(
        notes = page_data()$notes,
        assumptions = page_data()$assumptions,
        timestamp = data_timestamp()
      )),
      title = "Fundamentals notes"
    )

    output$page <- shiny::renderUI({
      ns <- session$ns
      current_filters <- filters()

      if (length(current_filters$commodities) == 0L) {
        return(htmltools::tagList(
          ea_selected_filters_ribbon(current_filters, data_timestamp()),
          ea_empty_state_card(
            title = "Select products to populate fundamentals",
            body = "Fundamentals is built for inventories, storage, crack spreads, treasury curves, and scheduled releases.",
            hint = "Use the shared product selection to populate the inventory, crack spread, and catalyst panels."
          ),
          mod_footer_notes_ui(ns("footer_notes"))
        ))
      }

      htmltools::tagList(
        ea_selected_filters_ribbon(current_filters, data_timestamp()),
        mod_kpi_strip_ui(ns("kpi_strip")),
        bslib::layout_columns(
          col_widths = c(8, 4),
          ea_plotly_card(
            "Inventory vs 5-Year Range",
            output_id = ns("inventory_seasonal"),
            subtitle = "Current year versus historical band by week.",
            height = "260px"
          ),
          ea_table_card(
            "Inventory Anomaly Summary",
            output_id = ns("inventory_anomaly_table"),
            subtitle = "Current versus 5-year average."
          )
        ),
        bslib::layout_columns(
          col_widths = c(6, 6),
          ea_plotly_card(
            "Storage Capacity Utilization",
            output_id = ns("storage_util_chart"),
            subtitle = "Current utilization vs 80%/95% thresholds.",
            height = "250px"
          ),
          ea_plotly_card(
            "Crack Spread Monitor",
            output_id = ns("crack_spread_chart"),
            subtitle = "RB-CL and HO-CL crack spreads over time.",
            height = "250px"
          )
        ),
        bslib::layout_columns(
          col_widths = c(8, 4),
          ea_plotly_card(
            "Treasury Yield Curve",
            output_id = ns("treasury_chart"),
            subtitle = "Current, 1-month, and 3-month ago curves.",
            height = "250px"
          ),
          ea_plotly_card(
            "Commodity-Rates Relationship",
            output_id = ns("comm_rate_scatter"),
            subtitle = "Daily log return vs 10Y UST change.",
            height = "250px"
          )
        ),
        bslib::layout_columns(
          col_widths = c(12),
          ea_table_card(
            "Release Calendar",
            output_id = ns("release_calendar_table"),
            subtitle = "Upcoming EIA data releases."
          )
        ),
        mod_footer_notes_ui(ns("footer_notes"))
      )
    })

    output$inventory_seasonal <- plotly::renderPlotly({
      ss <- page_data()$stocks_seasonal
      fig <- plotly::plot_ly()
      if (nrow(ss) == 0L) return(ea_plotly_layout(fig, x_title = "Week", y_title = "Level"))
      primary_prod <- ss$product[1]
      df <- ss[ss$product == primary_prod & !is.na(ss$avg_5yr), , drop = FALSE]
      locs <- unique(df$location)
      pref_loc <- if ("total" %in% tolower(locs)) locs[tolower(locs) == "total"][1] else locs[1]
      df <- df[df$location == pref_loc, , drop = FALSE]
      if (nrow(df) == 0L) return(ea_plotly_layout(fig, x_title = "Week", y_title = "Level"))
      fig <- fig |>
        plotly::add_ribbons(data = df, x = ~week, ymin = ~min_5yr, ymax = ~max_5yr,
          name = "5Y range", line = list(color = "transparent"),
          fillcolor = "rgba(77,163,163,0.12)", showlegend = TRUE, hoverinfo = "skip") |>
        plotly::add_lines(data = df, x = ~week, y = ~avg_5yr,
          name = "5Y avg", line = list(color = "#7f8b99", width = 1.5, dash = "dot"),
          hovertemplate = "Wk %{x}<br>5Y avg: %{y:.1f}<extra></extra>") |>
        plotly::add_lines(data = df[!is.na(df$current), , drop = FALSE],
          x = ~week, y = ~current,
          name = "Current", line = list(color = "#4da3a3", width = 2.4),
          hovertemplate = "Wk %{x}<br>Current: %{y:.1f}<extra></extra>")
      ea_plotly_layout(fig, x_title = "Week of year", y_title = "Inventory level")
    })

    output$inventory_anomaly_table <- reactable::renderReactable({
      sd_df <- page_data()$stocks_deviation
      if (nrow(sd_df) == 0L) return(reactable::reactable(tibble::tibble(), theme = ea_reactable_theme()))
      latest <- sd_df |>
        dplyr::arrange(dplyr::desc(.data$date)) |>
        dplyr::group_by(.data$product) |>
        dplyr::slice(1L) |>
        dplyr::ungroup() |>
        dplyr::mutate(
          deviation = round(.data$deviation, 1),
          deviation_pct = scales::percent(.data$deviation_pct, accuracy = 0.1)
        ) |>
        dplyr::select(.data$product, .data$deviation, .data$deviation_pct, .data$date)
      reactable::reactable(
        latest,
        theme = ea_reactable_theme(),
        striped = TRUE, highlight = TRUE,
        columns = list(
          product = reactable::colDef(name = "Product"),
          deviation = reactable::colDef(name = "vs 5Y Avg"),
          deviation_pct = reactable::colDef(name = "% Dev"),
          date = reactable::colDef(name = "Date", format = reactable::colFormat(date = TRUE))
        )
      )
    })

    output$storage_util_chart <- plotly::renderPlotly({
      sc <- page_data()$storage_capacity_util
      fig <- plotly::plot_ly()
      if (nrow(sc) == 0L) return(ea_plotly_layout(fig, x_title = NULL, y_title = "Utilization %"))
      bar_colors <- dplyr::case_when(
        sc$utilization_pct >= 0.95 ~ "#b35c60",
        sc$utilization_pct >= 0.80 ~ "#d2a157",
        TRUE ~ "#4da3a3"
      )
      fig <- plotly::plot_ly(data = sc, x = ~product, y = ~utilization_pct,
        type = "bar", marker = list(color = bar_colors),
        hovertemplate = "%{x}<br>Utilization: %{y:.1%}<extra></extra>") |>
        plotly::layout(shapes = list(
          list(type = "line", x0 = -0.5, x1 = nrow(sc) - 0.5, y0 = 0.80, y1 = 0.80,
               line = list(color = "#d2a157", width = 1.5, dash = "dash")),
          list(type = "line", x0 = -0.5, x1 = nrow(sc) - 0.5, y0 = 0.95, y1 = 0.95,
               line = list(color = "#b35c60", width = 1.5, dash = "dash"))
        ),
        yaxis = list(tickformat = ".0%"))
      ea_plotly_layout(fig, x_title = NULL, y_title = "Utilization", hovermode = "closest")
    })

    output$crack_spread_chart <- plotly::renderPlotly({
      cs <- page_data()$crack_spreads
      palette <- c("#4da3a3", "#d2a157", "#5a85c8")
      fig <- plotly::plot_ly()
      if (nrow(cs) == 0L) return(ea_plotly_layout(fig, x_title = NULL, y_title = "Crack spread"))
      spreads <- unique(cs$spread_label)
      for (i in seq_along(spreads)) {
        sp <- spreads[[i]]
        df <- cs[cs$spread_label == sp, , drop = FALSE]
        col <- palette[[((i - 1L) %% length(palette)) + 1L]]
        fig <- fig |>
          plotly::add_lines(data = df, x = ~date, y = ~crack_value,
            name = sp, line = list(color = col, width = 1.8),
            hovertemplate = "%{x|%d %b %Y}<br>%{y:.2f}<extra>%{fullData.name}</extra>")
      }
      ea_plotly_layout(fig, x_title = NULL, y_title = "Crack spread ($/bbl)")
    })

    output$treasury_chart <- plotly::renderPlotly({
      ts_df <- page_data()$treasury_snapshot
      palette <- c(current = "#4da3a3", m1_ago = "#d2a157", m3_ago = "#7f8b99")
      labels_map <- c(current = "Current", m1_ago = "1M ago", m3_ago = "3M ago")
      fig <- plotly::plot_ly()
      if (nrow(ts_df) == 0L) return(ea_plotly_layout(fig, x_title = "Tenor (years)", y_title = "Yield (%)"))
      for (lbl in c("current", "m1_ago", "m3_ago")) {
        df <- ts_df[ts_df$snapshot_label == lbl, , drop = FALSE]
        if (nrow(df) == 0L) next
        df <- df[order(df$curve_point_num), , drop = FALSE]
        col <- palette[[lbl]]
        fig <- fig |>
          plotly::add_lines(data = df, x = ~curve_point_num, y = ~yield,
            name = labels_map[[lbl]], line = list(color = col, width = 2),
            hovertemplate = "Tenor %{x}Y<br>Yield: %{y:.2f}%<extra>%{fullData.name}</extra>")
      }
      ea_plotly_layout(fig, x_title = "Tenor (years)", y_title = "Yield (%)")
    })

    output$comm_rate_scatter <- plotly::renderPlotly({
      crs <- page_data()$commodity_rate_scatter
      palette <- ea_market_palette()
      fig <- plotly::plot_ly()
      if (nrow(crs) == 0L) return(ea_plotly_layout(fig, x_title = "10Y UST chg (bps)", y_title = "Commodity log return"))
      crs_recent <- crs |>
        dplyr::arrange(.data$date) |>
        dplyr::group_by(.data$market) |>
        dplyr::slice_tail(n = 252L) |>
        dplyr::ungroup()
      for (mkt in unique(crs_recent$market)) {
        df <- crs_recent[crs_recent$market == mkt, , drop = FALSE]
        col <- if (!is.null(palette[[mkt]])) unname(palette[[mkt]]) else "#4da3a3"
        fig <- fig |>
          plotly::add_markers(data = df, x = ~rate_chg, y = ~commodity_chg,
            name = mkt, marker = list(color = col, size = 4, opacity = 0.5),
            hovertemplate = "%{x:.3f} bps chg<br>%{y:.3f} log ret<extra>%{fullData.name}</extra>")
      }
      ea_plotly_layout(fig, x_title = "10Y UST daily chg", y_title = "Commodity log return", hovermode = "closest")
    })

    output$release_calendar_table <- reactable::renderReactable({
      rc <- page_data()$release_calendar
      if (nrow(rc) == 0L) return(reactable::reactable(tibble::tibble(), theme = ea_reactable_theme()))
      reactable::reactable(
        rc |> dplyr::mutate(release_date = format(.data$release_date, "%d %b %Y")),
        theme = ea_reactable_theme(),
        striped = TRUE, highlight = TRUE,
        columns = list(
          release_date = reactable::colDef(name = "Date"),
          event = reactable::colDef(name = "Release"),
          day_of_week = reactable::colDef(name = "Day")
        )
      )
    })
  })
}
