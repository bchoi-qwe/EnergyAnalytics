ea_term_structure_layout <- function(fig, x_title = NULL, y_title = NULL, hovermode = "x unified") {
  ea_plotly_layout(fig, x_title = x_title, y_title = y_title, hovermode = hovermode) %>%
    plotly::layout(
      xaxis = list(fixedrange = TRUE),
      yaxis = list(fixedrange = TRUE)
    )
}

ea_forward_standardize <- function(x) {
  finite_x <- x[is.finite(x)]
  if (length(finite_x) < 2L) {
    return(rep(NA_real_, length(x)))
  }

  x_sd <- stats::sd(finite_x)
  if (!is.finite(x_sd) || x_sd == 0) {
    return(rep(NA_real_, length(x)))
  }

  (x - mean(finite_x)) / x_sd
}

mod_forward_curves_ui <- function(id) {
  shiny::uiOutput(shiny::NS(id, "page"))
}

mod_forward_curves_server <- function(id, filters, data_timestamp) {
  shiny::moduleServer(id, function(input, output, session) {
    view_choices <- c(
      "Current" = "current",
      "History" = "history",
      "Heatmap" = "heatmap",
      "Cross-Market" = "comparison",
      "Prompt" = "prompt",
      "Rates" = "rates"
    )
    history_basis_choices <- c(
      "Trailing" = "trailing",
      "Full" = "full"
    )
    history_year_choices <- ea_history_year_choices()

    selected_context_basis <- shiny::reactive({
      current <- ea_coalesce(input$forward_context_basis, "trailing")
      if (current %in% unname(history_basis_choices)) current else "trailing"
    })

    selected_context_years <- shiny::reactive({
      current <- suppressWarnings(as.integer(ea_coalesce(input$forward_context_years, "5")[[1]]))
      if (!is.finite(current) || is.na(current) || current < 1L) {
        5L
      } else {
        min(current, 30L)
      }
    })

    selected_context <- shiny::reactive({
      if (identical(selected_context_basis(), "full")) {
        "full"
      } else {
        paste0(selected_context_years(), "y")
      }
    })

    page_data <- shiny::reactive(
      ea_calc_forward_curves(filters(), history_context = selected_context())
    )

    selected_market <- shiny::reactive({
      available_markets <- page_data()$available_markets

      if (length(available_markets) == 0L) {
        return(NA_character_)
      }

      current <- ea_coalesce(
        input$term_market,
        ea_coalesce(page_data()$focus_market, available_markets[[1]])
      )

      if (current %in% available_markets) {
        current
      } else {
        available_markets[[1]]
      }
    })

    selected_view <- shiny::reactive({
      current <- ea_coalesce(input$forward_view, "current")
      if (current %in% unname(view_choices)) current else "current"
    })

    selected_curve <- shiny::reactive({
      chart_data <- page_data()$curve_snapshot %>%
        dplyr::filter(.data$market == selected_market())

      if (nrow(chart_data) == 0L) {
        return(chart_data)
      }

      front_price <- chart_data$price[[which.min(chart_data$curve_point_num)]]

      chart_data %>%
        dplyr::mutate(
          front_price = front_price,
          delta_front = .data$price - front_price,
          tenor_label = paste0("M", .data$curve_point_num)
        )
    })

    selected_history <- shiny::reactive({
      page_data()$curve_history %>%
        dplyr::filter(.data$market == selected_market())
    })

    selected_heatmap <- shiny::reactive({
      page_data()$curve_change_heatmap %>%
        dplyr::filter(.data$market == selected_market()) %>%
        dplyr::arrange(.data$curve_point_num, .data$date)
    })

    selected_spread_strip <- shiny::reactive({
      page_data()$calendar_spread_strip %>%
        dplyr::filter(.data$market == selected_market()) %>%
        dplyr::arrange(.data$front_tenor)
    })

    indexed_curve_comparison <- shiny::reactive({
      page_data()$curve_snapshot %>%
        dplyr::group_by(.data$market) %>%
        dplyr::mutate(
          front_price = .data$price[[which.min(.data$curve_point_num)]],
          curve_index = dplyr::if_else(
            is.finite(.data$front_price) & .data$front_price != 0,
            (.data$price / .data$front_price) * 100,
            NA_real_
          )
        ) %>%
        dplyr::ungroup() %>%
        dplyr::filter(is.finite(.data$curve_index))
    })

    prompt_spread_chart <- shiny::reactive({
      page_data()$prompt_spread_ts %>%
        dplyr::filter(is.finite(.data$zscore)) %>%
        dplyr::arrange(.data$market, .data$date)
    })

    selected_treasury_overlay <- shiny::reactive({
      page_data()$treasury_overlay %>%
        dplyr::filter(.data$market == selected_market()) %>%
        dplyr::arrange(.data$date)
    })

    selected_treasury_overlay_chart <- shiny::reactive({
      overlay_data <- selected_treasury_overlay()

      if (nrow(overlay_data) == 0L) {
        return(overlay_data)
      }

      overlay_data %>%
        dplyr::mutate(
          spread_z = ea_forward_standardize(.data$cmdty_spread),
          rate_z = ea_forward_standardize(.data$slope_2s10s)
        )
    })

    selected_heatmap_summary <- shiny::reactive({
      heatmap_data <- selected_heatmap()
      tenor_focus <- c(1, 3, 6, 12)

      purrr::map_dfr(tenor_focus, function(tenor) {
        df <- heatmap_data %>%
          dplyr::filter(.data$curve_point_num == tenor) %>%
          dplyr::arrange(.data$date)

        if (nrow(df) == 0L) {
          return(tibble::tibble())
        }

        latest_value <- df$value[[nrow(df)]]
        chg_5d <- if (nrow(df) >= 6L) latest_value - df$value[[nrow(df) - 5L]] else NA_real_
        chg_20d <- if (nrow(df) >= 21L) latest_value - df$value[[nrow(df) - 20L]] else NA_real_

        tibble::tibble(
          tenor = paste0("M", tenor),
          level = latest_value,
          chg_5d = chg_5d,
          chg_20d = chg_20d
        )
      })
    })

    selected_view_spec <- shiny::reactive({
      switch(
        selected_view(),
        current = list(
          plot_title = "Current Curve",
          plot_output = session$ns("curve_snapshot"),
          table_title = "Summary",
          table_output = session$ns("structure_table"),
          plot_height = "320px"
        ),
        history = list(
          plot_title = "Curve History",
          plot_output = session$ns("curve_history"),
          table_title = "History Table",
          table_output = session$ns("curve_history_table"),
          plot_height = "320px"
        ),
        heatmap = list(
          plot_title = "Curve Heatmap",
          plot_output = session$ns("curve_heatmap"),
          table_title = "Tenor Change",
          table_output = session$ns("curve_heatmap_table"),
          plot_height = "340px"
        ),
        comparison = list(
          plot_title = "Curve Comparison",
          plot_output = session$ns("curve_comparison"),
          table_title = "Market Table",
          table_output = session$ns("curve_comparison_table"),
          plot_height = "320px"
        ),
        prompt = list(
          plot_title = "Prompt Stress",
          plot_output = session$ns("prompt_spread_zscore"),
          table_title = "Spread Ladder",
          table_output = session$ns("spread_strip_table"),
          plot_height = "320px"
        ),
        rates = list(
          plot_title = "Rates Linkage",
          plot_output = session$ns("treasury_overlay"),
          table_title = "Rates Table",
          table_output = session$ns("rates_overlay_table"),
          plot_height = "320px"
        )
      )
    })

    output$page <- shiny::renderUI({
      ns <- session$ns
      available_markets <- page_data()$available_markets
      selected_choice <- selected_market()
      selected_view_choice <- selected_view()
      selected_context_basis_choice <- selected_context_basis()
      selected_context_years_choice <- as.character(selected_context_years())
      years_visible_condition <- sprintf("input['%s'] !== 'full'", ns("forward_context_basis"))

      if (length(available_markets) == 0L) {
        return(htmltools::tagList(
          ea_empty_state_card(
            title = "No curves",
            body = "Adjust filters."
          )
        ))
      }

      htmltools::tagList(
        htmltools::tags$div(
          class = "ea-toolbar",
          htmltools::tags$div(
            class = "ea-toolbar__row",
            htmltools::tags$div(
              class = "ea-toolbar__field ea-toolbar__field--sm",
              ea_toolbar_select(
                input_id = ns("term_market"),
                label = "Market",
                choices = stats::setNames(available_markets, available_markets),
                selected = selected_choice
              )
            ),
            htmltools::tags$div(
              class = "ea-toolbar__field ea-toolbar__field--sm",
              ea_toolbar_select(
                input_id = ns("forward_view"),
                label = "View",
                choices = view_choices,
                selected = selected_view_choice
              )
            ),
            htmltools::tags$div(
              class = "ea-toolbar__field ea-toolbar__field--sm",
              ea_toolbar_select(
                input_id = ns("forward_context_basis"),
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
                  input_id = ns("forward_context_years"),
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

    output$curve_snapshot <- plotly::renderPlotly({
      chart_data <- selected_curve()
      fig <- plotly::plot_ly()

      if (nrow(chart_data) == 0L) {
        return(ea_term_structure_layout(fig, x_title = "MTH", y_title = "PX"))
      }

      color <- unname(ea_market_palette()[[selected_market()]]) %||% ea_accent_color()
      key_points <- chart_data %>%
        dplyr::filter(.data$curve_point_num %in% c(1, 6, 12))

      fig <- fig %>%
        plotly::add_trace(
          data = chart_data,
          x = ~curve_point_num,
          y = ~price,
          type = "scatter",
          mode = "lines+markers",
          name = selected_market(),
          showlegend = FALSE,
          line = list(width = 2.6, color = color),
          marker = list(size = 5, color = color, line = list(color = "#0f1822", width = 1)),
          customdata = ~delta_front,
          hovertemplate = "M%{x}<br>PX: %{y:.2f}<br>vs M1: %{customdata:.2f}<extra></extra>"
        )

      if (nrow(key_points) > 0L) {
        fig <- fig %>%
          plotly::add_markers(
            data = key_points,
            x = ~curve_point_num,
            y = ~price,
            inherit = FALSE,
            showlegend = FALSE,
            marker = list(size = 9, color = color, line = list(color = "#e6edf5", width = 1.2)),
            hovertemplate = "M%{x}<br>PX: %{y:.2f}<extra></extra>"
          )
      }

      fig %>%
        ea_term_structure_layout(x_title = "MTH", y_title = "PX") %>%
        plotly::layout(
          shapes = list(
            list(
              type = "line",
              x0 = min(chart_data$curve_point_num),
              x1 = max(chart_data$curve_point_num),
              y0 = chart_data$front_price[[1]],
              y1 = chart_data$front_price[[1]],
              line = list(color = "rgba(230,237,245,0.22)", width = 1, dash = "dot")
            )
          )
        )
    })

    output$structure_table <- reactable::renderReactable({
      summary_data <- page_data()$structure_summary

      if (is.null(summary_data) || nrow(summary_data) == 0L) {
        return(reactable::reactable(
          tibble::tibble(),
          pagination = FALSE,
          theme = ea_reactable_theme()
        ))
      }

      focus_summary <- summary_data %>%
        dplyr::filter(.data$market == selected_market()) %>%
        dplyr::slice(1)

      if (nrow(focus_summary) == 0L) {
        return(reactable::reactable(
          tibble::tibble(),
          pagination = FALSE,
          theme = ea_reactable_theme()
        ))
      }

      format_num <- function(x, accuracy = 0.01) {
        if (isTRUE(is.finite(x))) {
          scales::number(x, accuracy = accuracy)
        } else {
          "N/A"
        }
      }

      format_pct <- function(x, accuracy = 0.1) {
        if (isTRUE(is.finite(x))) {
          scales::percent(x, accuracy = accuracy)
        } else {
          "N/A"
        }
      }

      metric_tone <- function(metric, value, regime = NULL) {
        if (identical(metric, "Regime")) {
          if (is.null(regime) || is.na(regime) || identical(regime, "Flat")) {
            return("neutral")
          }

          if (grepl("Backwardation", regime, fixed = TRUE) || grepl("Inverted", regime, fixed = TRUE)) {
            return("positive")
          }

          if (grepl("Contango", regime, fixed = TRUE)) {
            return("warning")
          }

          return("accent")
        }

        if (!is.finite(value)) {
          return("neutral")
        }

        if (identical(metric, "Prompt Z")) {
          if (abs(value) >= 2) return("accent")
          if (abs(value) >= 1) return("warning")
          return("neutral")
        }

        if (identical(metric, "Curve Pct")) {
          if (value >= 0.85 || value <= 0.15) {
            return("accent")
          }
          return("neutral")
        }

        if (identical(metric, "Regime Freq")) {
          if (value <= 0.20) return("accent")
          if (value <= 0.35) return("warning")
          return("neutral")
        }

        if (value > 0) {
          return("positive")
        }

        if (value < 0) {
          return("warning")
        }

        "neutral"
      }

      regime_value <- focus_summary$regime[[1]]
      history_label <- page_data()$history_context_label
      display <- tibble::tribble(
        ~metric, ~context, ~value, ~tone,
        "Front", "M1", format_num(focus_summary$m1_price[[1]]), "neutral",
        "Prompt", "M1-M2", format_num(focus_summary$m1_m2[[1]]), metric_tone("Prompt", focus_summary$m1_m2[[1]]),
        "Prompt Z", history_label, format_num(focus_summary$prompt_zscore[[1]]), metric_tone("Prompt Z", focus_summary$prompt_zscore[[1]]),
        "Back slope", "M1-M12", format_num(focus_summary$m1_m12[[1]]), metric_tone("Back slope", focus_summary$m1_m12[[1]]),
        "Curve Pct", history_label, format_pct(focus_summary$m1_m12_percentile[[1]], accuracy = 1), metric_tone("Curve Pct", focus_summary$m1_m12_percentile[[1]]),
        "Regime", "", regime_value, metric_tone("Regime", NA_real_, regime = regime_value),
        "Regime Freq", history_label, format_pct(focus_summary$regime_share[[1]], accuracy = 1), metric_tone("Regime Freq", focus_summary$regime_share[[1]]),
        "Roll", "", format_pct(focus_summary$roll_yield_ann[[1]]), metric_tone("Roll", focus_summary$roll_yield_ann[[1]])
      )

      reactable::reactable(
        display,
        pagination = FALSE,
        compact = TRUE,
        striped = FALSE,
        highlight = FALSE,
        borderless = TRUE,
        sortable = FALSE,
        resizable = FALSE,
        class = "ea-summary-table",
        theme = ea_reactable_theme(),
        defaultColDef = reactable::colDef(
          headerStyle = list(whiteSpace = "nowrap"),
          style = list(whiteSpace = "nowrap", borderColor = "#1a2432")
        ),
        columns = list(
          metric = reactable::colDef(
            minWidth = 150,
            cell = function(value, index) {
              htmltools::tags$div(
                class = "ea-summary-metric",
                htmltools::tags$div(class = "ea-summary-metric__label", value),
                if (nzchar(display$context[[index]])) {
                  htmltools::tags$div(class = "ea-summary-metric__context", display$context[[index]])
                }
              )
            }
          ),
          context = reactable::colDef(show = FALSE),
          value = reactable::colDef(
            align = "right",
            minWidth = 150,
            cell = function(value, index) {
              htmltools::tags$span(
                class = paste("ea-summary-value", paste0("ea-summary-value--", display$tone[[index]])),
                value
              )
            }
          ),
          tone = reactable::colDef(show = FALSE)
        ),
        rowStyle = list(minHeight = "48px")
      )
    })

    output$curve_history <- plotly::renderPlotly({
      chart_data <- selected_history()
      colors <- c("NOW" = "#4da3a3", "1M" = "#d2a157", "3M" = "#7f8b99")
      dashes <- c("NOW" = "solid", "1M" = "dash", "3M" = "dot")
      fig <- plotly::plot_ly()

      if (nrow(chart_data) == 0L) {
        return(ea_term_structure_layout(fig, x_title = "MTH", y_title = "PX"))
      }

      for (snap in names(colors)) {
        df <- chart_data[chart_data$snapshot_label == snap, , drop = FALSE]
        if (nrow(df) == 0L) {
          next
        }

        snap_date <- format(df$snapshot_date[[1]], "%d %b %y")
        fig <- fig %>%
          plotly::add_lines(
            data = df,
            x = ~curve_point_num,
            y = ~price,
            name = paste0(snap, " · ", snap_date),
            line = list(
              width = if (identical(snap, "NOW")) 2.5 else 1.8,
              color = unname(colors[[snap]]),
              dash = unname(dashes[[snap]])
            ),
            hovertemplate = "M%{x}<br>PX: %{y:.2f}<extra>%{fullData.name}</extra>"
          )
      }

      ea_term_structure_layout(fig, x_title = "MTH", y_title = "PX")
    })

    output$curve_history_table <- reactable::renderReactable({
      history_tbl <- selected_history() %>%
        dplyr::group_by(.data$snapshot_label, .data$snapshot_date) %>%
        dplyr::summarise(
          front = .data$price[[which.min(.data$curve_point_num)]],
          prompt = {
            front_tenor <- min(.data$curve_point_num, na.rm = TRUE)
            front_val <- .data$price[match(front_tenor, .data$curve_point_num)]
            next_val <- .data$price[match(front_tenor + 1L, .data$curve_point_num)]
            if (length(next_val) == 0L || is.na(next_val)) NA_real_ else front_val - next_val
          },
          back = {
            front_tenor <- min(.data$curve_point_num, na.rm = TRUE)
            front_val <- .data$price[match(front_tenor, .data$curve_point_num)]
            back_val <- .data$price[match(12L, .data$curve_point_num)]
            if (length(back_val) == 0L || is.na(back_val)) NA_real_ else front_val - back_val
          },
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          snapshot_order = match(.data$snapshot_label, c("NOW", "1M", "3M")),
          snap = .data$snapshot_label,
          date = format(.data$snapshot_date, "%d %b %y")
        ) %>%
        dplyr::arrange(.data$snapshot_order) %>%
        dplyr::select("snap", "date", "front", "prompt", "back")

      if (nrow(history_tbl) == 0L) {
        return(ea_empty_reactable())
      }

      reactable::reactable(
        history_tbl,
        pagination = FALSE,
        compact = TRUE,
        highlight = TRUE,
        theme = ea_reactable_theme(),
        columns = list(
          snap = reactable::colDef(name = "Snap", maxWidth = 72),
          date = reactable::colDef(name = "Date"),
          front = reactable::colDef(name = "Front", format = reactable::colFormat(digits = 2)),
          prompt = reactable::colDef(name = "M1-M2", format = reactable::colFormat(digits = 2)),
          back = reactable::colDef(name = "M1-M12", format = reactable::colFormat(digits = 2))
        )
      )
    })

    output$curve_heatmap <- plotly::renderPlotly({
      heatmap_data <- selected_heatmap()
      fig <- plotly::plot_ly()

      if (nrow(heatmap_data) == 0L) {
        return(ea_plotly_layout(fig, x_title = "DATE", y_title = "MTH", hovermode = "closest"))
      }

      fig %>%
        plotly::add_heatmap(
          data = heatmap_data,
          x = ~date,
          y = ~curve_point_num,
          z = ~value,
          colors = c("#14202d", "#235068", "#4da3a3", "#d2a157", "#f1c970"),
          colorbar = list(title = "PX"),
          customdata = ~change,
          hovertemplate = "%{x|%d %b %Y}<br>M%{y}<br>PX: %{z:.2f}<br>1D Chg: %{customdata:.2f}<extra></extra>"
        ) %>%
        ea_plotly_layout(x_title = "DATE", y_title = "MTH", hovermode = "closest") %>%
        plotly::layout(
          yaxis = list(
            title = list(text = "MTH", font = list(size = 10, color = "#6f7d8c")),
            color = "#9aa6b2",
            tickmode = "array",
            tickvals = sort(unique(heatmap_data$curve_point_num)),
            ticktext = paste0("M", sort(unique(heatmap_data$curve_point_num))),
            gridcolor = "rgba(148, 163, 184, 0.08)"
          )
        )
    })

    output$curve_heatmap_table <- reactable::renderReactable({
      display <- selected_heatmap_summary()

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
          tenor = reactable::colDef(name = "Tenor"),
          level = reactable::colDef(name = "Level", format = reactable::colFormat(digits = 2)),
          chg_5d = reactable::colDef(name = "5D", format = reactable::colFormat(digits = 2)),
          chg_20d = reactable::colDef(name = "20D", format = reactable::colFormat(digits = 2))
        )
      )
    })

    output$curve_comparison <- plotly::renderPlotly({
      chart_data <- indexed_curve_comparison()
      fig <- plotly::plot_ly()

      if (nrow(chart_data) == 0L) {
        return(ea_term_structure_layout(fig, x_title = "MTH", y_title = "Index"))
      }

      palette <- ea_market_palette()
      market_levels <- unique(chart_data$market)

      for (mkt in market_levels) {
        df <- chart_data[chart_data$market == mkt, , drop = FALSE]
        if (nrow(df) == 0L) {
          next
        }

        is_focus <- identical(mkt, selected_market())
        fig <- fig %>%
          plotly::add_lines(
            data = df,
            x = ~curve_point_num,
            y = ~curve_index,
            name = mkt,
            line = list(
              width = if (is_focus) 2.8 else 1.6,
              color = unname(palette[[mkt]]) %||% ea_accent_color(),
              dash = if (is_focus) "solid" else "dot"
            ),
            hovertemplate = paste0(mkt, "<br>M%{x}<br>Index: %{y:.1f}<extra></extra>")
          )
      }

      fig %>%
        ea_term_structure_layout(x_title = "MTH", y_title = "Index") %>%
        plotly::layout(
          shapes = list(
            list(
              type = "line",
              x0 = min(chart_data$curve_point_num),
              x1 = max(chart_data$curve_point_num),
              y0 = 100,
              y1 = 100,
              line = list(color = "rgba(148,163,184,0.35)", width = 1, dash = "dash")
            )
          ),
          legend = list(
            orientation = "h",
            y = -0.18,
            x = 0
          )
        )
    })

    output$curve_comparison_table <- reactable::renderReactable({
      display <- page_data()$structure_summary %>%
        dplyr::transmute(
          market = .data$market,
          front = .data$m1_price,
          prompt = .data$m1_m2,
          pz = .data$prompt_zscore,
          pct = .data$m1_m12_percentile,
          regime = .data$regime
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
          front = reactable::colDef(name = "Front", format = reactable::colFormat(digits = 2)),
          prompt = reactable::colDef(name = "M1-M2", format = reactable::colFormat(digits = 2)),
          pz = reactable::colDef(name = "Pz", format = reactable::colFormat(digits = 2)),
          pct = reactable::colDef(name = "Pct", format = reactable::colFormat(percent = TRUE, digits = 0)),
          regime = reactable::colDef(name = "Regime", minWidth = 120)
        )
      )
    })

    output$spread_strip_table <- reactable::renderReactable({
      strip_data <- selected_spread_strip()

      if (nrow(strip_data) == 0L) {
        return(reactable::reactable(
          tibble::tibble(),
          pagination = FALSE,
          theme = ea_reactable_theme()
        ))
      }

      display <- strip_data %>%
        dplyr::transmute(
          spread = .data$spread_label,
          current = .data$current_spread,
          z = .data$zscore,
          pct = .data$percentile,
          signal = .data$rich_cheap
        )

      reactable::reactable(
        display,
        pagination = FALSE,
        compact = TRUE,
        striped = FALSE,
        highlight = TRUE,
        theme = ea_reactable_theme(),
        columns = list(
          spread = reactable::colDef(name = "Spread"),
          current = reactable::colDef(name = "Current", format = reactable::colFormat(digits = 2)),
          z = reactable::colDef(name = "Z", format = reactable::colFormat(digits = 2)),
          pct = reactable::colDef(name = "Pct", format = reactable::colFormat(percent = TRUE, digits = 0)),
          signal = reactable::colDef(name = "Signal")
        )
      )
    })

    output$prompt_spread_zscore <- plotly::renderPlotly({
      ts_data <- prompt_spread_chart()
      fig <- plotly::plot_ly()

      if (nrow(ts_data) == 0L) {
        return(ea_term_structure_layout(fig, x_title = "DATE", y_title = "Z-Score"))
      }

      palette <- ea_market_palette()
      market_levels <- unique(ts_data$market)

      for (mkt in market_levels) {
        df <- ts_data[ts_data$market == mkt, , drop = FALSE]
        if (nrow(df) == 0L) {
          next
        }

        is_focus <- identical(mkt, selected_market())
        fig <- fig %>%
          plotly::add_lines(
            data = df,
            x = ~date,
            y = ~zscore,
            name = mkt,
            line = list(
              width = if (is_focus) 2.8 else 1.5,
              color = unname(palette[[mkt]]) %||% ea_accent_color(),
              dash = if (is_focus) "solid" else "dot"
            ),
            hovertemplate = paste0(mkt, "<br>%{x|%d %b %Y}<br>Z: %{y:.2f}<extra></extra>")
          )

        fig <- fig %>%
          plotly::add_markers(
            data = utils::tail(df, 1L),
            x = ~date,
            y = ~zscore,
            inherit = FALSE,
            showlegend = FALSE,
            marker = list(
              size = 8,
              color = unname(palette[[mkt]]) %||% ea_accent_color(),
              line = list(color = "#e6edf5", width = 1)
            ),
            hovertemplate = paste0(mkt, "<br>%{x|%d %b %Y}<br>Z: %{y:.2f}<extra></extra>")
          )
      }

      fig %>%
        plotly::layout(
          shapes = list(
            list(
              type = "rect",
              x0 = min(ts_data$date),
              x1 = max(ts_data$date),
              y0 = -1,
              y1 = 1,
              fillcolor = "rgba(77,163,163,0.08)",
              line = list(color = "rgba(0,0,0,0)")
            ),
            list(
              type = "line",
              x0 = min(ts_data$date),
              x1 = max(ts_data$date),
              y0 = 0,
              y1 = 0,
              line = list(color = "rgba(148,163,184,0.4)", width = 1, dash = "dash")
            ),
            list(
              type = "line",
              x0 = min(ts_data$date),
              x1 = max(ts_data$date),
              y0 = 2,
              y1 = 2,
              line = list(color = "rgba(210,161,87,0.32)", width = 1, dash = "dot")
            ),
            list(
              type = "line",
              x0 = min(ts_data$date),
              x1 = max(ts_data$date),
              y0 = -2,
              y1 = -2,
              line = list(color = "rgba(210,161,87,0.32)", width = 1, dash = "dot")
            )
          )
        ) %>%
        ea_term_structure_layout(x_title = "DATE", y_title = "Z-Score")
    })

    output$rates_overlay_table <- reactable::renderReactable({
      overlay_data <- selected_treasury_overlay()

      if (nrow(overlay_data) == 0L) {
        return(ea_empty_reactable())
      }

      valid <- overlay_data %>%
        dplyr::filter(is.finite(.data$cmdty_spread), is.finite(.data$slope_2s10s))

      if (nrow(valid) == 0L) {
        return(ea_empty_reactable())
      }

      latest_row <- utils::tail(valid, 1L)
      trailing <- utils::tail(valid, min(63L, nrow(valid)))
      corr_63d <- if (nrow(trailing) >= 5L) stats::cor(trailing$cmdty_spread, trailing$slope_2s10s) else NA_real_
      beta_63d <- if (nrow(trailing) >= 5L && stats::var(trailing$slope_2s10s) > 0) {
        stats::cov(trailing$cmdty_spread, trailing$slope_2s10s) / stats::var(trailing$slope_2s10s)
      } else {
        NA_real_
      }
      spread_change_20d <- if (nrow(valid) >= 21L) latest_row$cmdty_spread[[1]] - valid$cmdty_spread[[nrow(valid) - 20L]] else NA_real_
      slope_change_20d <- if (nrow(valid) >= 21L) latest_row$slope_2s10s[[1]] - valid$slope_2s10s[[nrow(valid) - 20L]] else NA_real_

      display <- tibble::tribble(
        ~metric, ~value,
        "Date", format(latest_row$date[[1]], "%d %b %Y"),
        "Spread", scales::number(latest_row$cmdty_spread[[1]], accuracy = 0.01),
        "UST 2s10s", scales::number(latest_row$slope_2s10s[[1]], accuracy = 0.01),
        "20D Spread", if (is.finite(spread_change_20d)) scales::number(spread_change_20d, accuracy = 0.01) else "N/A",
        "20D 2s10s", if (is.finite(slope_change_20d)) scales::number(slope_change_20d, accuracy = 0.01) else "N/A",
        "63D Corr", if (is.finite(corr_63d)) scales::number(corr_63d, accuracy = 0.01) else "N/A",
        "63D Beta", if (is.finite(beta_63d)) scales::number(beta_63d, accuracy = 0.01) else "N/A"
      )

      reactable::reactable(
        display,
        pagination = FALSE,
        compact = TRUE,
        highlight = FALSE,
        theme = ea_reactable_theme(),
        columns = list(
          metric = reactable::colDef(name = "Metric"),
          value = reactable::colDef(name = "Value")
        )
      )
    })

    output$treasury_overlay <- plotly::renderPlotly({
      overlay_data <- selected_treasury_overlay_chart()
      fig <- plotly::plot_ly()

      if (nrow(overlay_data) == 0L) {
        return(ea_term_structure_layout(fig, x_title = "DATE", y_title = "Std. Score"))
      }

      fig %>%
        plotly::add_lines(
          data = overlay_data,
          x = ~date,
          y = ~spread_z,
          name = "Commodity",
          line = list(color = "#4da3a3", width = 2.4),
          hovertemplate = "%{x|%d %b %Y}<br>Commodity z: %{y:.2f}<extra></extra>"
        ) %>%
        plotly::add_lines(
          data = overlay_data,
          x = ~date,
          y = ~rate_z,
          name = "UST 2s10s",
          line = list(color = "#d2a157", width = 1.9, dash = "dash"),
          hovertemplate = "%{x|%d %b %Y}<br>2s10s z: %{y:.2f}<extra></extra>"
        ) %>%
        ea_term_structure_layout(x_title = "DATE", y_title = "Std. Score") %>%
        plotly::layout(
          shapes = list(
            list(
              type = "line",
              x0 = min(overlay_data$date),
              x1 = max(overlay_data$date),
              y0 = 0,
              y1 = 0,
              line = list(color = "rgba(148,163,184,0.35)", width = 1, dash = "dash")
            )
          )
        )
    })
  })
}
