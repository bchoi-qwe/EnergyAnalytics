mod_volatility_ui <- function(id) {
  shiny::uiOutput(shiny::NS(id, "page"))
}

mod_volatility_server <- function(id, filters, data_timestamp) {
  shiny::moduleServer(id, function(input, output, session) {
    view_choices <- c(
      "History" = "history",
      "Cone" = "cone",
      "Term" = "term"
    )
    history_basis_choices <- c(
      "Trailing" = "trailing",
      "Full" = "full"
    )
    history_year_choices <- ea_history_year_choices()

    selected_context_basis <- shiny::reactive({
      current <- ea_coalesce(input$vol_context_basis, "trailing")
      if (current %in% unname(history_basis_choices)) current else "trailing"
    })

    selected_context_years <- shiny::reactive({
      current <- suppressWarnings(as.integer(ea_coalesce(input$vol_context_years, "5")[[1]]))
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
      ea_calc_volatility(filters(), history_context = selected_context())
    )

    available_markets <- shiny::reactive({
      mkts <- page_data()$available_markets
      mkts <- mkts[nzchar(mkts)]
      if (length(mkts) == 0L) {
        mkts <- ea_coalesce(filters()$commodities, character(0))
      }
      unique(mkts)
    })

    selected_market <- shiny::reactive({
      current_selection <- ea_coalesce(input$vol_market, character(0))
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
      current <- ea_coalesce(input$vol_view, "history")
      if (current %in% unname(view_choices)) current else "history"
    })

    selected_history_series <- shiny::reactive({
      current_market <- selected_market()
      rv_ts <- page_data()$realized_vol_timeseries
      iv_ts <- page_data()$realized_vs_implied

      if (length(current_market) != 1L) {
        return(list(rv20 = rv_ts[0, ], rv60 = rv_ts[0, ], iv = iv_ts[0, ]))
      }

      list(
        rv20 = rv_ts %>% dplyr::filter(.data$market == current_market, .data$window == "20d"),
        rv60 = rv_ts %>% dplyr::filter(.data$market == current_market, .data$window == "60d"),
        iv = iv_ts %>% dplyr::filter(.data$market == current_market, is.finite(.data$atm_iv))
      )
    })

    selected_history_context <- shiny::reactive({
      current_market <- selected_market()
      page_data()$vol_history_context %>%
        dplyr::filter(.data$market == current_market)
    })

    selected_cone <- shiny::reactive({
      current_market <- selected_market()
      page_data()$vol_cone %>%
        dplyr::filter(.data$market == current_market) %>%
        dplyr::arrange(.data$horizon)
    })

    selected_term <- shiny::reactive({
      current_market <- selected_market()
      page_data()$vol_term_structure %>%
        dplyr::filter(.data$market == current_market) %>%
        dplyr::arrange(.data$tenor)
    })

    selected_atm_term <- shiny::reactive({
      current_market <- selected_market()
      surface <- page_data()$vol_surface_grid %>%
        dplyr::filter(.data$market == current_market)

      if (nrow(surface) == 0L) {
        return(tibble::tibble(tenor = numeric(), atm_iv = numeric()))
      }

      surface %>%
        dplyr::group_by(.data$curve_point_num) %>%
        dplyr::slice_min(abs(.data$moneyness - 1), n = 1L, with_ties = FALSE) %>%
        dplyr::ungroup() %>%
        dplyr::transmute(tenor = .data$curve_point_num, atm_iv = .data$iv) %>%
        dplyr::arrange(.data$tenor)
    })

    selected_history_table <- shiny::reactive({
      current_market <- selected_market()
      history_context <- selected_history_context()
      regime <- page_data()$vol_regime %>% dplyr::filter(.data$market == current_market)
      iv_rv <- page_data()$realized_vs_implied %>%
        dplyr::filter(.data$market == current_market) %>%
        dplyr::arrange(.data$date)
      vov <- page_data()$vol_of_vol_context %>% dplyr::filter(.data$market == current_market)

      rv20 <- history_context %>% dplyr::filter(.data$window == "20d")
      rv60 <- history_context %>% dplyr::filter(.data$window == "60d")
      latest_iv <- if (nrow(iv_rv) > 0L) utils::tail(iv_rv$atm_iv[is.finite(iv_rv$atm_iv)], 1L) else NA_real_
      latest_spread <- if (nrow(iv_rv) > 0L) utils::tail(iv_rv$iv_rv_spread[is.finite(iv_rv$iv_rv_spread)], 1L) else NA_real_

      tibble::tribble(
        ~metric, ~context, ~value,
        "20d RV", page_data()$history_context_label, if (nrow(rv20) > 0L) scales::percent(rv20$current_vol[[1]], accuracy = 0.1) else "N/A",
        "20d Pct", page_data()$history_context_label, if (nrow(rv20) > 0L) scales::percent(rv20$percentile[[1]], accuracy = 1) else "N/A",
        "20d Z", page_data()$history_context_label, if (nrow(rv20) > 0L && is.finite(rv20$zscore[[1]])) scales::number(rv20$zscore[[1]], accuracy = 0.01) else "N/A",
        "60d RV", "annualized", if (nrow(rv60) > 0L) scales::percent(rv60$current_vol[[1]], accuracy = 0.1) else "N/A",
        "ATM IV", "front listed", if (length(latest_iv) > 0L && is.finite(latest_iv[[1]])) scales::percent(latest_iv[[1]], accuracy = 0.1) else "N/A",
        "IV-RV", "ATM - 60d", if (length(latest_spread) > 0L && is.finite(latest_spread[[1]])) paste0(ifelse(latest_spread[[1]] >= 0, "+", ""), scales::percent(latest_spread[[1]], accuracy = 0.1)) else "N/A",
        "VoV", page_data()$history_context_label, if (nrow(vov) > 0L && is.finite(vov$current_vov[[1]])) scales::percent(vov$current_vov[[1]], accuracy = 0.1) else "N/A",
        "Regime", "", if (nrow(regime) > 0L) regime$regime[[1]] else "N/A"
      )
    })

    selected_view_spec <- shiny::reactive({
      switch(
        selected_view(),
        history = list(
          plot_title = "Vol History",
          plot_output = session$ns("vol_history_chart"),
          table_title = "Vol Summary",
          table_output = session$ns("vol_history_table"),
          plot_height = "320px"
        ),
        cone = list(
          plot_title = "Vol Cone",
          plot_output = session$ns("vol_cone"),
          table_title = "Cone Table",
          table_output = session$ns("vol_cone_table"),
          plot_height = "320px"
        ),
        term = list(
          plot_title = "Vol Term",
          plot_output = session$ns("vol_term_chart"),
          table_title = "Term Table",
          table_output = session$ns("vol_term_table"),
          plot_height = "320px"
        )
      )
    })

    output$page <- shiny::renderUI({
      ns <- session$ns
      current_filters <- filters()
      selected_choice <- selected_market()
      selected_view_choice <- selected_view()
      selected_context_basis_choice <- selected_context_basis()
      selected_context_years_choice <- as.character(selected_context_years())
      years_visible_condition <- sprintf("input['%s'] !== 'full'", ns("vol_context_basis"))

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
            htmltools::tags$div(
              class = "ea-toolbar__field ea-toolbar__field--sm",
              ea_toolbar_select(
                input_id = ns("vol_market"),
                label = "Market",
                choices = stats::setNames(available_markets(), ea_market_labels(available_markets())),
                selected = selected_choice
              )
            ),
            htmltools::tags$div(
              class = "ea-toolbar__field ea-toolbar__field--sm",
              ea_toolbar_select(
                input_id = ns("vol_view"),
                label = "View",
                choices = view_choices,
                selected = selected_view_choice
              )
            ),
            htmltools::tags$div(
              class = "ea-toolbar__field ea-toolbar__field--sm",
              ea_toolbar_select(
                input_id = ns("vol_context_basis"),
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
                  input_id = ns("vol_context_years"),
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

    output$vol_history_chart <- plotly::renderPlotly({
      series <- selected_history_series()
      context_rows <- selected_history_context()
      rv20 <- series$rv20
      rv60 <- series$rv60
      iv_ts <- series$iv
      fig <- plotly::plot_ly()

      if (nrow(rv20) == 0L && nrow(rv60) == 0L && nrow(iv_ts) == 0L) {
        return(ea_plotly_layout(fig, x_title = NULL, y_title = "Annualized Vol"))
      }

      band_source <- if (nrow(rv20) > 0L) rv20 else rv60
      rv20_context <- context_rows %>% dplyr::filter(.data$window == "20d")

      if (nrow(band_source) > 0L && nrow(rv20_context) > 0L) {
        band_df <- tibble::tibble(
          date = band_source$date,
          p10 = rv20_context$p10[[1]],
          p25 = rv20_context$p25[[1]],
          p75 = rv20_context$p75[[1]],
          p90 = rv20_context$p90[[1]],
          p50 = rv20_context$p50[[1]]
        )

        fig <- fig %>%
          plotly::add_ribbons(
            data = band_df,
            x = ~date,
            ymin = ~p10,
            ymax = ~p90,
            fillcolor = "rgba(77,163,163,0.10)",
            line = list(color = "transparent"),
            showlegend = FALSE,
            hoverinfo = "skip"
          ) %>%
          plotly::add_ribbons(
            data = band_df,
            x = ~date,
            ymin = ~p25,
            ymax = ~p75,
            fillcolor = "rgba(77,163,163,0.18)",
            line = list(color = "transparent"),
            showlegend = FALSE,
            hoverinfo = "skip"
          ) %>%
          plotly::add_lines(
            data = band_df,
            x = ~date,
            y = ~p50,
            name = paste0("20d median · ", page_data()$history_context_label),
            line = list(color = "#7f8b99", width = 1.2, dash = "dot"),
            hoverinfo = "skip"
          )
      }

      if (nrow(rv20) > 0L) {
        fig <- fig %>%
          plotly::add_lines(
            data = rv20,
            x = ~date,
            y = ~realized_vol,
            name = "20d RV",
            line = list(color = "#4da3a3", width = 2.3),
            hovertemplate = "%{x|%d %b %Y}<br>20d RV: %{y:.1%}<extra></extra>"
          )
      }

      if (nrow(rv60) > 0L) {
        fig <- fig %>%
          plotly::add_lines(
            data = rv60,
            x = ~date,
            y = ~realized_vol,
            name = "60d RV",
            line = list(color = "#5a85c8", width = 1.8, dash = "dash"),
            hovertemplate = "%{x|%d %b %Y}<br>60d RV: %{y:.1%}<extra></extra>"
          )
      }

      if (nrow(iv_ts) > 0L) {
        fig <- fig %>%
          plotly::add_lines(
            data = iv_ts,
            x = ~date,
            y = ~atm_iv,
            name = "ATM IV",
            line = list(color = "#d2a157", width = 1.8, dash = "dot"),
            hovertemplate = "%{x|%d %b %Y}<br>ATM IV: %{y:.1%}<extra></extra>"
          )
      }

      ea_plotly_layout(fig, x_title = NULL, y_title = "Annualized Vol")
    })

    output$vol_history_table <- reactable::renderReactable({
      display <- selected_history_table()

      reactable::reactable(
        display,
        pagination = FALSE,
        compact = TRUE,
        striped = FALSE,
        highlight = FALSE,
        borderless = TRUE,
        theme = ea_reactable_theme(),
        columns = list(
          metric = reactable::colDef(name = "Metric", minWidth = 110),
          context = reactable::colDef(name = "Read", minWidth = 90),
          value = reactable::colDef(name = "Value", minWidth = 100)
        )
      )
    })

    output$vol_cone <- plotly::renderPlotly({
      vc <- selected_cone()
      fig <- plotly::plot_ly()

      if (nrow(vc) == 0L) {
        return(ea_plotly_layout(fig, x_title = "Horizon (Days)", y_title = "Annualized Vol"))
      }

      fig %>%
        plotly::add_ribbons(
          data = vc,
          x = ~horizon,
          ymin = ~p10,
          ymax = ~p90,
          fillcolor = "rgba(77,163,163,0.12)",
          line = list(color = "transparent"),
          showlegend = FALSE,
          hoverinfo = "skip"
        ) %>%
        plotly::add_ribbons(
          data = vc,
          x = ~horizon,
          ymin = ~p25,
          ymax = ~p75,
          fillcolor = "rgba(77,163,163,0.24)",
          line = list(color = "transparent"),
          showlegend = FALSE,
          hoverinfo = "skip"
        ) %>%
        plotly::add_lines(
          data = vc,
          x = ~horizon,
          y = ~p50,
          name = paste0("Median · ", page_data()$history_context_label),
          line = list(color = "#7f8b99", width = 1.4, dash = "dot"),
          hoverinfo = "skip"
        ) %>%
        plotly::add_trace(
          data = vc,
          x = ~horizon,
          y = ~current_vol,
          type = "scatter",
          name = "Current",
          line = list(color = "#4da3a3", width = 2.4),
          marker = list(color = "#d2a157", size = 8),
          mode = "lines+markers",
          customdata = ~percentile,
          hovertemplate = "%{x}d<br>Current: %{y:.1%}<br>Pct: %{customdata:.0%}<extra></extra>"
        ) %>%
        ea_plotly_layout(fig, x_title = "Horizon (Days)", y_title = "Annualized Vol")
    })

    output$vol_cone_table <- reactable::renderReactable({
      vc <- selected_cone()

      if (nrow(vc) == 0L) {
        return(ea_empty_reactable())
      }

      display <- vc %>%
        dplyr::transmute(
          horizon = paste0(.data$horizon, "d"),
          current = .data$current_vol,
          pct = .data$percentile,
          z = .data$zscore,
          p25 = .data$p25,
          median = .data$p50,
          p75 = .data$p75
        )

      reactable::reactable(
        display,
        pagination = FALSE,
        compact = TRUE,
        highlight = TRUE,
        theme = ea_reactable_theme(),
        columns = list(
          horizon = reactable::colDef(name = "Horizon"),
          current = reactable::colDef(name = "Current", format = reactable::colFormat(percent = TRUE, digits = 1)),
          pct = reactable::colDef(name = "Pct", format = reactable::colFormat(percent = TRUE, digits = 0)),
          z = reactable::colDef(name = "Z", format = reactable::colFormat(digits = 2)),
          p25 = reactable::colDef(name = "P25", format = reactable::colFormat(percent = TRUE, digits = 1)),
          median = reactable::colDef(name = "Median", format = reactable::colFormat(percent = TRUE, digits = 1)),
          p75 = reactable::colDef(name = "P75", format = reactable::colFormat(percent = TRUE, digits = 1))
        )
      )
    })

    output$vol_term_chart <- plotly::renderPlotly({
      term_vol <- selected_term()
      atm_surface <- selected_atm_term()
      fig <- plotly::plot_ly()

      if (nrow(term_vol) == 0L && nrow(atm_surface) == 0L) {
        return(ea_plotly_layout(fig, x_title = "Contract Month", y_title = "Annualized Vol"))
      }

      if (nrow(term_vol) > 0L) {
        fig <- fig %>%
          plotly::add_lines(
            data = term_vol,
            x = ~tenor,
            y = ~rv20,
            name = "20d RV",
            line = list(color = "#4da3a3", width = 2.2),
            hovertemplate = "M%{x}<br>20d RV: %{y:.1%}<extra></extra>"
          ) %>%
          plotly::add_lines(
            data = term_vol,
            x = ~tenor,
            y = ~rv60,
            name = "60d RV",
            line = list(color = "#5a85c8", width = 1.8, dash = "dash"),
            hovertemplate = "M%{x}<br>60d RV: %{y:.1%}<extra></extra>"
          )
      }

      if (nrow(atm_surface) > 0L) {
        fig <- fig %>%
          plotly::add_trace(
            data = atm_surface,
            x = ~tenor,
            y = ~atm_iv,
            type = "scatter",
            mode = "lines+markers",
            name = "ATM IV",
            line = list(color = "#d2a157", width = 1.8, dash = "dot"),
            marker = list(color = "#d2a157", size = 8),
            hovertemplate = "M%{x}<br>ATM IV: %{y:.1%}<extra></extra>"
          )
      }

      ea_plotly_layout(fig, x_title = "Contract Month", y_title = "Annualized Vol")
    })

    output$vol_term_table <- reactable::renderReactable({
      term_vol <- selected_term()
      atm_surface <- selected_atm_term()

      display <- term_vol %>%
        dplyr::left_join(atm_surface, by = "tenor") %>%
        dplyr::mutate(
          iv_rv_60 = .data$atm_iv - .data$rv60
        ) %>%
        dplyr::arrange(.data$tenor)

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
          market = reactable::colDef(show = FALSE),
          tenor = reactable::colDef(name = "Tenor"),
          rv20 = reactable::colDef(name = "20d RV", format = reactable::colFormat(percent = TRUE, digits = 1)),
          rv60 = reactable::colDef(name = "60d RV", format = reactable::colFormat(percent = TRUE, digits = 1)),
          rv120 = reactable::colDef(name = "120d RV", format = reactable::colFormat(percent = TRUE, digits = 1)),
          realized_vol = reactable::colDef(show = FALSE),
          atm_iv = reactable::colDef(name = "ATM IV", format = reactable::colFormat(percent = TRUE, digits = 1)),
          iv_rv_60 = reactable::colDef(name = "IV-RV", format = reactable::colFormat(percent = TRUE, digits = 1))
        )
      )
    })
  })
}
