mod_seasonality_ui <- function(id) {
  shiny::uiOutput(shiny::NS(id, "page"))
}

mod_seasonality_server <- function(id, filters, data_timestamp) {
  shiny::moduleServer(id, function(input, output, session) {
    history_basis_choices <- c(
      "Trailing" = "trailing",
      "Full" = "full"
    )
    history_year_choices <- ea_history_year_choices()
    selected_context_basis <- shiny::reactive({
      current <- ea_coalesce(input$seasonal_context_basis, "trailing")
      if (current %in% unname(history_basis_choices)) current else "trailing"
    })
    selected_context_years <- shiny::reactive({
      current <- suppressWarnings(as.integer(ea_coalesce(input$seasonal_context_years, "5")[[1]]))
      if (!is.finite(current) || is.na(current) || current < 1L) {
        5L
      } else {
        min(current, 30L)
      }
    })
    selected_context <- shiny::reactive({
      if (identical(selected_context_basis(), "full")) "full" else paste0(selected_context_years(), "y")
    })
    page_data <- shiny::reactive({ ea_calc_seasonality(filters(), history_context = selected_context()) })
    view_choices <- c(
      "Overlay" = "overlay",
      "Year Compare" = "year_compare",
      "Spread" = "spread"
    )
    available_markets <- shiny::reactive({
      mkts <- unique(page_data()$seasonal_overlay$market)
      mkts <- mkts[nzchar(mkts)]
      if (length(mkts) == 0L) {
        mkts <- ea_coalesce(filters()$commodities, character(0))
      }
      unique(mkts)
    })
    focus_market <- shiny::reactive({
      current_selection <- ea_coalesce(input$seasonal_focus_market, character(0))
      market_choices <- available_markets()

      if (length(current_selection) == 1L && current_selection %in% market_choices) {
        current_selection
      } else if (length(market_choices) > 0L) {
        market_choices[[1]]
      } else {
        character(0)
      }
    })
    selected_view <- shiny::reactive({
      current <- ea_coalesce(input$seasonal_view, "overlay")
      if (current %in% unname(view_choices)) current else "overlay"
    })
    selected_view_spec <- shiny::reactive({
      switch(
        selected_view(),
        overlay = list(
          plot_title = "Seasonal Overlay",
          plot_output = session$ns("seasonal_overlay"),
          table_title = "Summary",
          table_output = session$ns("seasonal_summary_table"),
          plot_height = "340px"
        ),
        year_compare = list(
          plot_title = "Year-on-Year",
          plot_output = session$ns("yoy_compare"),
          table_title = "Year Table",
          table_output = session$ns("yoy_table"),
          plot_height = "340px"
        ),
        spread = list(
          plot_title = "Seasonal Spreads",
          plot_output = session$ns("seasonal_spread_chart"),
          table_title = "Spread Table",
          table_output = session$ns("seasonal_spread_table"),
          plot_height = "340px"
        )
      )
    })

    output$page <- shiny::renderUI({
      ns <- session$ns
      current_filters <- filters()
      selected_view_choice <- selected_view()
      selected_context_basis_choice <- selected_context_basis()
      selected_context_years_choice <- as.character(selected_context_years())
      focus_market_visible_condition <- sprintf("input['%s'] !== 'overlay'", ns("seasonal_view"))
      years_visible_condition <- sprintf("input['%s'] !== 'full'", ns("seasonal_context_basis"))

      if (length(current_filters$commodities) == 0L) {
        return(htmltools::tagList(
          ea_empty_state_card(
            title = "Select products",
            body = "Add products."
          )
        ))
      }

      htmltools::tagList(
        htmltools::tags$div(
          class = "ea-toolbar",
          htmltools::tags$div(
            class = "ea-toolbar__row",
            shiny::conditionalPanel(
              condition = focus_market_visible_condition,
              htmltools::tags$div(
                class = "ea-toolbar__field ea-toolbar__field--sm",
                ea_toolbar_select(
                  input_id = ns("seasonal_focus_market"),
                  label = "Focus Market",
                  choices = stats::setNames(available_markets(), ea_market_labels(available_markets())),
                  selected = focus_market()
                )
              )
            ),
            htmltools::tags$div(
              class = "ea-toolbar__field ea-toolbar__field--sm",
              ea_toolbar_select(
                input_id = ns("seasonal_view"),
                label = "View",
                choices = view_choices,
                selected = selected_view_choice
              )
            ),
            htmltools::tags$div(
              class = "ea-toolbar__field ea-toolbar__field--sm",
              ea_toolbar_select(
                input_id = ns("seasonal_context_basis"),
                label = "Basis",
                choices = history_basis_choices,
                selected = selected_context_basis_choice
              )
            ),
            shiny::conditionalPanel(
              condition = years_visible_condition,
              htmltools::tags$div(
                class = "ea-toolbar__field ea-toolbar__field--sm",
                ea_toolbar_select(
                  input_id = ns("seasonal_context_years"),
                  label = "Years",
                  choices = history_year_choices,
                  selected = selected_context_years_choice
                )
              )
            )
          )
        ),
        shiny::uiOutput(ns("view_panel"))
      )
    })

    output$view_panel <- shiny::renderUI({
      view_spec <- selected_view_spec()
      ea_module_view_panel(
        plot_title = view_spec$plot_title,
        plot_output_id = view_spec$plot_output,
        table_title = view_spec$table_title,
        table_output_id = view_spec$table_output,
        plot_height = view_spec$plot_height
      )
    })

    output$seasonal_summary_table <- reactable::renderReactable({
      summary_tbl <- page_data()$seasonal_summary %>%
        dplyr::mutate(
          market_label = ea_market_labels(.data$market),
          current_percentile = dplyr::if_else(
            is.finite(.data$current_percentile),
            scales::percent(.data$current_percentile, accuracy = 1),
            "N/A"
          ),
          anomaly_score = dplyr::if_else(
            is.finite(.data$anomaly_score),
            scales::number(.data$anomaly_score, accuracy = 0.01),
            "N/A"
          )
        ) %>%
        dplyr::select(
          market = .data$market_label,
          current_percentile = .data$current_percentile,
          anomaly_score = .data$anomaly_score,
          direction = .data$direction,
          days_to_inflection = .data$days_to_inflection
        )

      reactable::reactable(
        summary_tbl,
        pagination = FALSE,
        compact = TRUE,
        highlight = TRUE,
        theme = ea_reactable_theme(),
        columns = list(
          market = reactable::colDef(name = "Market"),
          current_percentile = reactable::colDef(name = "Pctile"),
          anomaly_score = reactable::colDef(name = "Anomaly"),
          direction = reactable::colDef(name = "Bias"),
          days_to_inflection = reactable::colDef(name = "Days")
        )
      )
    })

    output$yoy_table <- reactable::renderReactable({
      so <- page_data()$seasonal_overlay %>%
        dplyr::filter(.data$market == focus_market())

      display <- so %>%
        dplyr::group_by(.data$year) %>%
        dplyr::summarise(
          last_index = dplyr::last(.data$indexed_value),
          ytd_return = dplyr::last(.data$ytd_return),
          peak = max(.data$indexed_value, na.rm = TRUE),
          trough = min(.data$indexed_value, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::arrange(dplyr::desc(.data$year)) %>%
        utils::head(5)

      if (nrow(display) == 0L) {
        return(ea_empty_reactable())
      }

      reactable::reactable(
        display,
        pagination = FALSE,
        compact = TRUE,
        highlight = TRUE,
        theme = ea_reactable_theme(),
        columns = list(
          year = reactable::colDef(name = "Year"),
          last_index = reactable::colDef(name = "Last", format = reactable::colFormat(digits = 1)),
          ytd_return = reactable::colDef(name = "YTD", format = reactable::colFormat(percent = TRUE, digits = 1)),
          peak = reactable::colDef(name = "Peak", format = reactable::colFormat(digits = 1)),
          trough = reactable::colDef(name = "Trough", format = reactable::colFormat(digits = 1))
        )
      )
    })

    output$seasonal_spread_table <- reactable::renderReactable({
      ss <- page_data()$seasonal_spreads
      if (nrow(ss) == 0L) {
        return(ea_empty_reactable())
      }

      display <- ss %>%
        dplyr::filter(.data$market == focus_market()) %>%
        dplyr::slice_max(.data$day_of_year, n = 1L, with_ties = FALSE) %>%
        dplyr::transmute(
          market = .data$market,
          current = .data$current_spread,
          avg = .data$avg,
          p25 = .data$p25,
          p75 = .data$p75,
          percentile = .data$current_percentile
        )

      if (nrow(display) == 0L) {
        return(ea_empty_reactable())
      }

      reactable::reactable(
        display,
        pagination = FALSE,
        compact = TRUE,
        highlight = TRUE,
        theme = ea_reactable_theme(),
        columns = list(
          market = reactable::colDef(name = "Mkt", maxWidth = 72),
          current = reactable::colDef(name = "Current", format = reactable::colFormat(digits = 2)),
          avg = reactable::colDef(name = "Avg", format = reactable::colFormat(digits = 2)),
          p25 = reactable::colDef(name = "P25", format = reactable::colFormat(digits = 2)),
          p75 = reactable::colDef(name = "P75", format = reactable::colFormat(digits = 2)),
          percentile = reactable::colDef(name = "Pct", format = reactable::colFormat(percent = TRUE, digits = 1))
        )
      )
    })

    output$seasonal_overlay <- plotly::renderPlotly({
      sr <- page_data()$seasonal_range
      so <- page_data()$seasonal_overlay
      anchor_date <- page_data()$anchor_date
      palette <- ea_market_palette()
      seasonal_axis_date <- function(day_of_year) {
        as.Date("2000-01-01") + day_of_year - 1L
      }
      rgba_color <- function(color, alpha) {
        hex <- sub("^#", "", color)
        r_val <- strtoi(substr(hex, 1, 2), 16L)
        g_val <- strtoi(substr(hex, 3, 4), 16L)
        b_val <- strtoi(substr(hex, 5, 6), 16L)
        paste0("rgba(", r_val, ",", g_val, ",", b_val, ",", alpha, ")")
      }
      fig <- plotly::plot_ly()
      if (nrow(sr) == 0L && nrow(so) == 0L) {
        return(ea_plotly_layout(fig, x_title = "Month", y_title = "Index (100 = Jan 1)"))
      }

      markets <- unique(c(sr$market, so$market))
      markets <- markets[nzchar(markets)]
      for (mkt in markets) {
        df_r <- sr[sr$market == mkt, , drop = FALSE] %>%
          dplyr::arrange(.data$day_of_year) %>%
          dplyr::mutate(seasonal_date = seasonal_axis_date(.data$day_of_year))

        df_c <- so[so$market == mkt, , drop = FALSE]
        if (nrow(df_c) > 0L) {
          current_year <- max(df_c$year, na.rm = TRUE)
          df_c <- df_c %>%
            dplyr::filter(.data$year == current_year) %>%
            dplyr::arrange(.data$day_of_year) %>%
            dplyr::mutate(seasonal_date = seasonal_axis_date(.data$day_of_year))
        } else {
          current_year <- NA_integer_
        }

        col <- unname(palette[[mkt]])
        if (is.null(col) || !nzchar(col)) {
          col <- ea_accent_color()
        }

        market_label <- ea_market_labels(mkt)[[1]]
        fill_col <- rgba_color(col, 0.12)
        avg_col <- rgba_color(col, 0.65)

        if (nrow(df_r) > 0L) {
          fig <- fig %>%
            plotly::add_ribbons(data = df_r, x = ~seasonal_date, ymin = ~p25, ymax = ~p75,
              name = paste(market_label, "IQR"),
              legendgroup = mkt,
              line = list(color = "transparent"), fillcolor = fill_col,
              showlegend = FALSE, hoverinfo = "skip") %>%
            plotly::add_lines(data = df_r, x = ~seasonal_date, y = ~avg,
              name = paste(market_label, "Seasonal Avg"),
              legendgroup = mkt,
              line = list(color = avg_col, width = 1.3, dash = "dash"),
              showlegend = FALSE,
              hovertemplate = "%{x|%b %d}<br>Seasonal avg: %{y:.1f}<extra>%{fullData.name}</extra>")
        }

        if (nrow(df_c) > 0L) {
          current_name <- paste(market_label, current_year, "YTD")
          latest_point <- utils::tail(df_c, 1L)
          fig <- fig %>%
            plotly::add_lines(data = df_c, x = ~seasonal_date, y = ~indexed_value,
              name = current_name,
              legendgroup = mkt,
              line = list(color = col, width = 3),
              hovertemplate = "%{x|%b %d}<br>Current year: %{y:.1f}<extra>%{fullData.name}</extra>") %>%
            plotly::add_markers(data = latest_point, x = ~seasonal_date, y = ~indexed_value,
              name = current_name,
              legendgroup = mkt,
              showlegend = FALSE,
              marker = list(color = col, size = 8, line = list(color = "#dce4ec", width = 1.2)),
              hovertemplate = "%{x|%b %d}<br>Latest: %{y:.1f}<extra>%{fullData.name}</extra>")
        }
      }

      month_ticks <- seq(as.Date("2000-01-01"), as.Date("2000-12-01"), by = "1 month")
      anchor_shapes <- list()
      anchor_annotations <- list()

      if (length(anchor_date) == 1L && !is.na(anchor_date)) {
        anchor_x <- seasonal_axis_date(as.integer(format(anchor_date, "%j")))
        anchor_shapes <- list(list(
          type = "line",
          x0 = anchor_x,
          x1 = anchor_x,
          xref = "x",
          y0 = 0,
          y1 = 1,
          yref = "paper",
          line = list(color = "rgba(230, 237, 245, 0.22)", width = 1, dash = "dot")
        ))
        anchor_annotations <- list(list(
          x = anchor_x,
          y = 1.03,
          xref = "x",
          yref = "paper",
          xanchor = "left",
          showarrow = FALSE,
          text = paste("As of", format(anchor_date, "%b %d, %Y")),
          font = list(color = "#9aa6b2", size = 10)
        ))
      }

      ea_plotly_layout(fig, x_title = "Month", y_title = "Index (100 = Jan 1)") %>%
        plotly::layout(
          hovermode = "x",
          xaxis = list(
            title = list(text = "Month", font = list(size = 10, color = "#6f7d8c")),
            type = "date",
            range = c(as.Date("2000-01-01"), as.Date("2000-12-31")),
            tickvals = month_ticks,
            tickformat = "%b",
            hoverformat = "%b %d",
            color = "#9aa6b2",
            gridcolor = "rgba(148, 163, 184, 0.08)",
            zerolinecolor = "rgba(148, 163, 184, 0.10)"
          ),
          shapes = anchor_shapes,
          annotations = anchor_annotations
        )
    })

    output$yoy_compare <- plotly::renderPlotly({
      so <- page_data()$seasonal_overlay
      palette <- c("#4da3a3", "#d2a157", "#5a85c8", "#d36e70", "#8ecf8e", "#c46e73")
      fig <- plotly::plot_ly()
      if (nrow(so) == 0L) return(ea_plotly_layout(fig, x_title = "Day of year", y_title = "Index"))
      primary_mkt <- focus_market()
      df <- so[so$market == primary_mkt, , drop = FALSE]
      years <- sort(unique(df$year), decreasing = TRUE)
      years <- utils::head(years, 5L)
      for (i in seq_along(years)) {
        yr <- years[[i]]
        df_y <- df[df$year == yr, , drop = FALSE]
        col <- palette[[((i - 1L) %% length(palette)) + 1L]]
        fig <- fig %>%
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
      df <- ss %>% dplyr::filter(.data$market == focus_market())
      if (nrow(df) == 0L) return(ea_plotly_layout(fig, x_title = "Day of year", y_title = "M1-M2 spread"))
      col <- unname(palette[[focus_market()]]) %||% ea_accent_color()
      hex <- sub("^#", "", col)
      r_val <- strtoi(substr(hex, 1, 2), 16L)
      g_val <- strtoi(substr(hex, 3, 4), 16L)
      b_val <- strtoi(substr(hex, 5, 6), 16L)
      fill_col <- paste0("rgba(", r_val, ",", g_val, ",", b_val, ",0.20)")
      fig <- fig %>%
        plotly::add_ribbons(data = df, x = ~day_of_year, ymin = ~p25, ymax = ~p75,
          name = "IQR",
          line = list(color = "transparent"), fillcolor = fill_col,
          showlegend = FALSE, hoverinfo = "skip") %>%
        plotly::add_lines(data = df, x = ~day_of_year, y = ~avg,
          name = "Avg",
          line = list(color = col, width = 1.3, dash = "dot"),
          hovertemplate = "Day %{x}<br>Avg: %{y:.2f}<extra></extra>") %>%
        plotly::add_lines(data = df, x = ~day_of_year, y = ~current_spread,
          name = "Current Year",
          line = list(color = col, width = 2.3),
          hovertemplate = "Day %{x}<br>Current: %{y:.2f}<extra></extra>")
      ea_plotly_layout(fig, x_title = "Day of year", y_title = "M1-M2 spread")
    })

  })
}
