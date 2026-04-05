mod_seasonality_ui <- function(id) {
  shiny::uiOutput(shiny::NS(id, "page"))
}

mod_seasonality_server <- function(id, filters, data_timestamp) {
  shiny::moduleServer(id, function(input, output, session) {
    page_data <- shiny::reactive({ ea_calc_seasonality(filters()) })

    mod_kpi_strip_server("kpi_strip", kpis = shiny::reactive(page_data()$kpis))

    mod_footer_notes_server(
      "footer_notes",
      notes = shiny::reactive(list(
        notes = page_data()$notes,
        assumptions = page_data()$assumptions,
        timestamp = data_timestamp()
      )),
      title = "Seasonal notes"
    )

    output$page <- shiny::renderUI({
      ns <- session$ns
      current_filters <- filters()

      if (length(current_filters$commodities) == 0L) {
        return(htmltools::tagList(
          ea_selected_filters_ribbon(current_filters, data_timestamp()),
          ea_empty_state_card(
            title = "Select products to populate seasonal views",
            body = "Seasonality compares recurring structure in outrights, calendar spreads, and year-on-year paths.",
            hint = "Choose one or more products to compare recurring calendar structure across markets."
          ),
          mod_footer_notes_ui(ns("footer_notes"))
        ))
      }

      htmltools::tagList(
        ea_selected_filters_ribbon(current_filters, data_timestamp()),
        mod_kpi_strip_ui(ns("kpi_strip")),
        # Row 1
        bslib::layout_columns(
          col_widths = c(7, 5),
          ea_plotly_card(
            title = "Seasonal Overlay",
            subtitle = "Current year vs 5-year historical range.",
            output_id = ns("seasonal_overlay"),
            height = "330px"
          ),
          ea_plotly_card(
            title = "Calendar Heatmap",
            subtitle = "Month-by-year seasonal structure.",
            output_id = ns("seasonal_heatmap"),
            height = "330px"
          )
        ),
        # Row 2
        bslib::layout_columns(
          col_widths = c(12),
          ea_plotly_card(
            title = "STL Decomposition",
            subtitle = "Trend, seasonal, and remainder components (faceted).",
            output_id = ns("stl_decomposition"),
            height = "380px"
          )
        ),
        # Row 3
        bslib::layout_columns(
          col_widths = c(8, 4),
          ea_plotly_card(
            title = "Year-on-Year",
            subtitle = "Relative path comparison across recent years.",
            output_id = ns("yoy_compare"),
            height = "330px"
          ),
          ea_plotly_card(
            title = "Seasonal Spread Profile",
            subtitle = "M1-M2 spread by day-of-year with percentile bands.",
            output_id = ns("seasonal_spread_chart"),
            height = "330px"
          )
        ),
        # Row 4
        bslib::layout_columns(
          col_widths = c(6, 6),
          ea_plotly_card(
            title = "Seasonal Volatility",
            subtitle = "Realized vol by day-of-year with percentile bands.",
            output_id = ns("seasonal_vol_chart"),
            height = "300px"
          ),
          ea_plotly_card(
            title = "Hedge Effectiveness by Month",
            subtitle = "R-squared between primary market and benchmark by calendar month.",
            output_id = ns("seasonal_hedge_chart"),
            height = "300px"
          )
        ),
        # Row 5
        bslib::layout_columns(
          col_widths = c(12),
          ea_plotly_card(
            title = "Monthly Return Pattern",
            subtitle = "Average log return and hit rate by calendar month.",
            output_id = ns("seasonal_returns_chart"),
            height = "280px"
          )
        ),
        mod_footer_notes_ui(ns("footer_notes"))
      )
    })

    output$seasonal_overlay <- plotly::renderPlotly({
      sr <- page_data()$seasonal_range
      so <- page_data()$seasonal_overlay
      palette <- ea_market_palette()
      fig <- plotly::plot_ly()
      if (nrow(sr) == 0L) return(ea_plotly_layout(fig, x_title = "Day of year", y_title = "Index"))
      for (mkt in unique(sr$market)) {
        df_r <- sr[sr$market == mkt, , drop = FALSE]
        df_c <- so[so$market == mkt & so$year == max(so$year[so$market == mkt]), , drop = FALSE]
        col <- unname(palette[[mkt]])
        hex <- sub("^#", "", col)
        r_val <- strtoi(substr(hex, 1, 2), 16L)
        g_val <- strtoi(substr(hex, 3, 4), 16L)
        b_val <- strtoi(substr(hex, 5, 6), 16L)
        fill_col <- paste0("rgba(", r_val, ",", g_val, ",", b_val, ",0.15)")
        fig <- fig |>
          plotly::add_ribbons(data = df_r, x = ~day_of_year, ymin = ~min, ymax = ~max,
            name = paste(mkt, "5yr range"),
            line = list(color = "transparent"), fillcolor = fill_col,
            showlegend = TRUE, hoverinfo = "skip") |>
          plotly::add_lines(data = df_c, x = ~day_of_year, y = ~indexed_value,
            name = paste(mkt, max(so$year[so$market == mkt])),
            line = list(color = col, width = 2),
            hovertemplate = "Day %{x}<br>%{y:.1f}<extra>%{fullData.name}</extra>")
      }
      ea_plotly_layout(fig, x_title = "Day of year", y_title = "Index (100 = Jan 1)")
    })

    output$seasonal_heatmap <- plotly::renderPlotly({
      so <- page_data()$seasonal_overlay
      if (nrow(so) == 0L) return(ea_plotly_layout(plotly::plot_ly(), x_title = NULL, y_title = NULL))
      primary_mkt <- so$market[1]
      df <- so |>
        dplyr::filter(.data$market == primary_mkt) |>
        dplyr::mutate(
          month_num = as.integer(format(
            as.Date(paste0(.data$year, "-01-01")) + .data$day_of_year - 1L, "%m")),
          month_abbr = month.abb[.data$month_num]
        ) |>
        dplyr::group_by(.data$year, .data$month_abbr) |>
        dplyr::summarise(avg_index = mean(.data$indexed_value, na.rm = TRUE), .groups = "drop")
      if (nrow(df) == 0L) return(ea_plotly_layout(plotly::plot_ly(), x_title = NULL, y_title = NULL))
      mat <- stats::xtabs(avg_index ~ year + month_abbr, data = df)
      # reorder columns by calendar month
      col_order <- month.abb[month.abb %in% colnames(mat)]
      mat <- mat[, col_order, drop = FALSE]
      fig <- plotly::plot_ly(
        x = colnames(mat), y = rownames(mat), z = unclass(mat),
        type = "heatmap",
        colors = c("#b35c60", "#17202b", "#4da3a3"),
        hovertemplate = "%{y} %{x}<br>%{z:.1f}<extra>index</extra>"
      ) |> plotly::layout(yaxis = list(autorange = "reversed"))
      ea_plotly_layout(fig, x_title = NULL, y_title = NULL, hovermode = "closest")
    })

    output$stl_decomposition <- plotly::renderPlotly({
      stl <- page_data()$stl_decomposition
      if (nrow(stl) == 0L) {
        fig <- plotly::plot_ly() |>
          plotly::add_annotations(text = "Insufficient data for STL (requires 2+ years)", showarrow = FALSE)
        return(ea_plotly_layout(fig, x_title = NULL, y_title = NULL))
      }
      p_trend <- plotly::plot_ly(data = stl, x = ~date, y = ~trend, type = "scatter", mode = "lines",
        name = "Trend", line = list(color = "#4da3a3", width = 1.5),
        hovertemplate = "%{x|%d %b %Y}<br>Trend: %{y:.2f}<extra></extra>")
      p_seasonal <- plotly::plot_ly(data = stl, x = ~date, y = ~seasonal, type = "scatter", mode = "lines",
        name = "Seasonal", line = list(color = "#5a85c8", width = 1.5),
        hovertemplate = "%{x|%d %b %Y}<br>Seasonal: %{y:.2f}<extra></extra>")
      p_remainder <- plotly::plot_ly(data = stl, x = ~date, y = ~remainder, type = "scatter", mode = "lines",
        name = "Remainder", line = list(color = "#d2a157", width = 1.5),
        hovertemplate = "%{x|%d %b %Y}<br>Remainder: %{y:.2f}<extra></extra>")
      fig <- plotly::subplot(list(p_trend, p_seasonal, p_remainder), nrows = 3, shareX = TRUE, titleX = FALSE)
      ea_plotly_layout(fig, x_title = NULL, y_title = NULL)
    })

    output$yoy_compare <- plotly::renderPlotly({
      so <- page_data()$seasonal_overlay
      palette <- c("#4da3a3", "#d2a157", "#5a85c8", "#d36e70", "#8ecf8e", "#c46e73")
      fig <- plotly::plot_ly()
      if (nrow(so) == 0L) return(ea_plotly_layout(fig, x_title = "Day of year", y_title = "Index"))
      primary_mkt <- so$market[1]
      df <- so[so$market == primary_mkt, , drop = FALSE]
      years <- sort(unique(df$year), decreasing = TRUE)
      years <- utils::head(years, 5L)
      for (i in seq_along(years)) {
        yr <- years[[i]]
        df_y <- df[df$year == yr, , drop = FALSE]
        col <- palette[[((i - 1L) %% length(palette)) + 1L]]
        fig <- fig |>
          plotly::add_lines(data = df_y, x = ~day_of_year, y = ~indexed_value,
            name = as.character(yr),
            line = list(color = col, width = if (i == 1L) 2.5 else 1.5),
            hovertemplate = "Day %{x}<br>%{y:.1f}<extra>%{fullData.name}</extra>")
      }
      ea_plotly_layout(fig, x_title = "Day of year", y_title = "Index")
    })

    output$seasonal_spread_chart <- plotly::renderPlotly({
      ss <- page_data()$seasonal_spreads
      palette <- ea_market_palette()
      fig <- plotly::plot_ly()
      if (nrow(ss) == 0L) return(ea_plotly_layout(fig, x_title = "Day of year", y_title = "M1-M2 spread"))
      for (mkt in unique(ss$market)) {
        df <- ss[ss$market == mkt, , drop = FALSE]
        col <- unname(palette[[mkt]])
        hex <- sub("^#", "", col)
        r_val <- strtoi(substr(hex, 1, 2), 16L)
        g_val <- strtoi(substr(hex, 3, 4), 16L)
        b_val <- strtoi(substr(hex, 5, 6), 16L)
        fill_col <- paste0("rgba(", r_val, ",", g_val, ",", b_val, ",0.20)")
        fig <- fig |>
          plotly::add_ribbons(data = df, x = ~day_of_year, ymin = ~p25, ymax = ~p75,
            name = paste(mkt, "IQR"),
            line = list(color = "transparent"), fillcolor = fill_col,
            showlegend = FALSE, hoverinfo = "skip") |>
          plotly::add_lines(data = df, x = ~day_of_year, y = ~avg,
            name = mkt, line = list(color = col, width = 2),
            hovertemplate = "Day %{x}<br>%{y:.2f}<extra>%{fullData.name}</extra>")
      }
      ea_plotly_layout(fig, x_title = "Day of year", y_title = "M1-M2 spread")
    })

    output$seasonal_vol_chart <- plotly::renderPlotly({
      sv <- page_data()$seasonal_vol
      palette <- ea_market_palette()
      fig <- plotly::plot_ly()
      if (nrow(sv) == 0L) return(ea_plotly_layout(fig, x_title = "Day of year", y_title = "Realized vol"))
      for (mkt in unique(sv$market)) {
        df <- sv[sv$market == mkt, , drop = FALSE]
        col <- unname(palette[[mkt]])
        hex <- sub("^#", "", col)
        r_val <- strtoi(substr(hex, 1, 2), 16L)
        g_val <- strtoi(substr(hex, 3, 4), 16L)
        b_val <- strtoi(substr(hex, 5, 6), 16L)
        fill_col <- paste0("rgba(", r_val, ",", g_val, ",", b_val, ",0.15)")
        fig <- fig |>
          plotly::add_ribbons(data = df, x = ~day_of_year, ymin = ~p25_vol, ymax = ~p75_vol,
            name = paste(mkt, "IQR"),
            line = list(color = "transparent"), fillcolor = fill_col,
            showlegend = FALSE, hoverinfo = "skip") |>
          plotly::add_lines(data = df, x = ~day_of_year, y = ~avg_vol,
            name = mkt, line = list(color = col, width = 2),
            hovertemplate = "Day %{x}<br>%{y:.1%}<extra>%{fullData.name}</extra>")
      }
      ea_plotly_layout(fig, x_title = "Day of year", y_title = "Realized vol (ann.)")
    })

    output$seasonal_hedge_chart <- plotly::renderPlotly({
      she <- page_data()$seasonal_hedge_effectiveness
      palette <- ea_market_palette()
      fig <- plotly::plot_ly()
      if (nrow(she) == 0L) {
        return(ea_plotly_layout(fig, x_title = "Month", y_title = "R-squared"))
      }
      # Order months properly
      month_order <- month.abb
      she <- she |> dplyr::mutate(month = factor(.data$month, levels = month_order))
      for (mkt in unique(she$market)) {
        df <- she[she$market == mkt, , drop = FALSE]
        col <- if (!is.null(palette[[mkt]])) unname(palette[[mkt]]) else "#4da3a3"
        fig <- fig |>
          plotly::add_lines(data = df, x = ~month, y = ~r_squared,
            name = mkt, line = list(color = col, width = 2),
            hovertemplate = "%{x}<br>R\u00b2: %{y:.2f}<extra>%{fullData.name}</extra>")
      }
      ea_plotly_layout(fig, x_title = "Month", y_title = "R-squared")
    })

    output$seasonal_returns_chart <- plotly::renderPlotly({
      sret <- page_data()$seasonal_returns
      fig <- plotly::plot_ly()
      if (nrow(sret) == 0L) return(ea_plotly_layout(fig, x_title = "Month", y_title = "Avg return"))
      primary_mkt <- sret$market[1]
      df <- sret[sret$market == primary_mkt, , drop = FALSE]
      month_order <- month.abb
      df <- df |> dplyr::mutate(period = factor(.data$period, levels = month_order)) |> dplyr::arrange(.data$period)
      bar_colors <- ifelse(df$avg_return >= 0, "#4da3a3", "#b35c60")
      fig <- plotly::plot_ly(data = df, x = ~period, y = ~avg_return, type = "bar",
        marker = list(color = bar_colors),
        hovertemplate = "%{x}<br>Avg return: %{y:.2%}<extra></extra>")
      ea_plotly_layout(fig, x_title = "Month", y_title = "Avg log return")
    })
  })
}
