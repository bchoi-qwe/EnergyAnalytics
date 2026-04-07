mod_hedging_ui <- function(id) {
  shiny::uiOutput(shiny::NS(id, "page"))
}

mod_hedging_server <- function(id, filters, data_timestamp) {
  shiny::moduleServer(id, function(input, output, session) {
    view_choices <- c(
      "Cross Hedge" = "cross",
      "Term Structure" = "term",
      "Rolling" = "rolling"
    )
    history_basis_choices <- c(
      "Trailing" = "trailing",
      "Full" = "full"
    )
    history_year_choices <- ea_history_year_choices()
    empty_page_data <- function(error_message = NULL) {
      list(
        hedge_ratios = tibble::tibble(market = character(), beta = numeric(), r_squared = numeric(), std_error = numeric()),
        ratio_stability = tibble::tibble(market = character(), beta_sd = numeric()),
        kpis = tibble::tibble(title = character(), value = character(), delta = character(), status = character()),
        cross_hedge_matrix = tibble::tibble(market_x = character(), market_y = character(), beta = numeric(), r_squared = numeric()),
        hedge_effectiveness = tibble::tibble(market = character(), unhedged_vol = numeric(), hedged_vol = numeric(), vol_reduction_pct = numeric()),
        rolling_beta = tibble::tibble(date = as.Date(character()), market = character(), beta = numeric(), r_squared = numeric()),
        residual_with_bands = tibble::tibble(
          date = as.Date(character()), market = character(), residual = numeric(), ou_mu = numeric(),
          band_1_lo = numeric(), band_1_hi = numeric(), band_2_lo = numeric(), band_2_hi = numeric()
        ),
        .error = error_message
      )
    }

    available_benchmarks <- shiny::reactive({
      markets <- unique(ea_coalesce(filters()$commodities, character(0)))
      markets[nzchar(markets)]
    })

    selected_benchmark <- shiny::reactive({
      benchmark_choices <- available_benchmarks()
      current_selection <- ea_coalesce(input$hedge_benchmark, character(0))

      if (length(current_selection) == 1L && current_selection %in% benchmark_choices) {
        current_selection
      } else if (length(benchmark_choices) > 0L) {
        benchmark_choices[[1]]
      } else {
        character(0)
      }
    })

    hedging_filters <- shiny::reactive({
      current_filters <- filters()
      current_filters$comparison_commodity <- selected_benchmark()
      current_filters
    })

    selected_context_basis <- shiny::reactive({
      current <- ea_coalesce(input$hedge_context_basis, "trailing")
      if (current %in% unname(history_basis_choices)) current else "trailing"
    })

    selected_context_years <- shiny::reactive({
      current <- suppressWarnings(as.integer(ea_coalesce(input$hedge_context_years, "5")[[1]]))
      if (!is.finite(current) || is.na(current) || current < 1L) {
        5L
      } else {
        min(current, 30L)
      }
    })

    selected_context <- shiny::reactive({
      if (identical(selected_context_basis(), "full")) "full" else paste0(selected_context_years(), "y")
    })

    page_data <- shiny::reactive({
      tryCatch(
        {
          result <- ea_calc_hedging(hedging_filters(), history_context = selected_context())
          result$.error <- NULL
          result
        },
        error = function(e) empty_page_data(conditionMessage(e))
      )
    })
    available_targets <- shiny::reactive({
      mkts <- unique(page_data()$rolling_beta$market)
      mkts <- mkts[nzchar(mkts)]
      if (length(mkts) == 0L) {
        mkts <- unique(page_data()$hedge_effectiveness$market)
      }
      unique(mkts)
    })
    focus_target <- shiny::reactive({
      current_selection <- ea_coalesce(input$hedge_focus_market, character(0))
      target_choices <- available_targets()

      if (length(current_selection) == 1L && current_selection %in% target_choices) {
        current_selection
      } else if (length(target_choices) > 0L) {
        target_choices[[1]]
      } else {
        character(0)
      }
    })
    selected_view <- shiny::reactive({
      current <- ea_coalesce(input$hedge_view, "cross")
      if (current %in% unname(view_choices)) current else "cross"
    })
    selected_view_spec <- shiny::reactive({
      switch(
        selected_view(),
        cross = list(
          plot_title = "Hedge Effectiveness",
          plot_output = session$ns("hedge_effectiveness"),
          table_title = "Hedge Ratios",
          table_output = session$ns("cross_hedge_table"),
          plot_height = "320px"
        ),
        term = list(
          plot_title = "Hedge Ratio by Tenor",
          plot_output = session$ns("per_tenor_ratios"),
          table_title = "Tenor Table",
          table_output = session$ns("per_tenor_table"),
          plot_height = "320px"
        ),
        rolling = list(
          plot_title = "Rolling Hedge Ratio",
          plot_output = session$ns("rolling_hedge_ratio"),
          table_title = "Rolling Table",
          table_output = session$ns("rolling_hedge_table"),
          plot_height = "320px"
        )
      )
    })

    output$page <- shiny::renderUI({
      ns <- session$ns
      current_filters <- filters()
      selected_view_choice <- selected_view()
      selected_context_basis_choice <- selected_context_basis()
      selected_context_years_choice <- as.character(selected_context_years())
      focus_target_visible_condition <- sprintf("input['%s'] !== 'cross'", ns("hedge_view"))
      years_visible_condition <- sprintf("input['%s'] !== 'full'", ns("hedge_context_basis"))

      if (length(current_filters$commodities) < 2L) {
        return(htmltools::tagList(
          ea_empty_state_card(
            title = "Select two products",
            body = "Add another product."
          )
        ))
      }

      if (!is.null(page_data()$.error)) {
        return(htmltools::tagList(
          ea_empty_state_card(
            title = "Data unavailable",
            body = "Current payload failed.",
            hint = page_data()$.error
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
                input_id = ns("hedge_benchmark"),
                label = "Benchmark",
                choices = stats::setNames(available_benchmarks(), ea_market_labels(available_benchmarks())),
                selected = selected_benchmark()
              )
            ),
            shiny::conditionalPanel(
              condition = focus_target_visible_condition,
              htmltools::tags$div(
                class = "ea-toolbar__field ea-toolbar__field--sm",
                ea_toolbar_select(
                  input_id = ns("hedge_focus_market"),
                  label = "Focus Target",
                  choices = stats::setNames(available_targets(), ea_market_labels(available_targets())),
                  selected = focus_target()
                )
              )
            ),
            htmltools::tags$div(
              class = "ea-toolbar__field ea-toolbar__field--sm",
              ea_toolbar_select(
                input_id = ns("hedge_view"),
                label = "View",
                choices = view_choices,
                selected = selected_view_choice
              )
            ),
            htmltools::tags$div(
              class = "ea-toolbar__field ea-toolbar__field--sm",
              ea_toolbar_select(
                input_id = ns("hedge_context_basis"),
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
                  input_id = ns("hedge_context_years"),
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

    # --- Row 1 left: Cross-Hedge Ranking Table ---
    output$cross_hedge_table <- reactable::renderReactable({
      mat <- page_data()$hedge_ratios %>%
        dplyr::left_join(
          page_data()$hedge_effectiveness %>%
            dplyr::select(.data$market, .data$vol_reduction_pct),
          by = "market"
        ) %>%
        dplyr::left_join(
          page_data()$stress_period_comparison %>%
            dplyr::mutate(stress_delta = .data$r2_stress - .data$r2_normal) %>%
            dplyr::select(.data$market, .data$stress_delta),
          by = "market"
        ) %>%
        dplyr::left_join(page_data()$ratio_stability, by = "market") %>%
        dplyr::mutate(
          basis_risk = 1 - .data$r_squared,
          market_label = ea_market_labels(.data$market),
          r_squared_fmt = scales::percent(.data$r_squared, accuracy = 0.1),
          basis_risk_fmt = scales::number(.data$basis_risk, accuracy = 0.01),
          beta_fmt = scales::number(.data$beta, accuracy = 0.01),
          vol_reduction_fmt = scales::percent(.data$vol_reduction_pct, accuracy = 1),
          beta_sd_fmt = scales::number(.data$beta_sd, accuracy = 0.01),
          stress_delta_fmt = dplyr::if_else(
            is.finite(.data$stress_delta),
            scales::number(.data$stress_delta, accuracy = 0.01),
            "N/A"
          )
        )

      if (nrow(mat) == 0L) {
        return(ea_empty_reactable())
      }

      reactable::reactable(
        mat,
        theme = ea_reactable_theme(),
        compact = TRUE,
        sortable = TRUE,
        defaultSorted = list(r_squared = "desc"),
        columns = list(
          market      = reactable::colDef(show = FALSE),
          market_label = reactable::colDef(name = "Target"),
          beta_fmt   = reactable::colDef(name = "Beta"),
          r_squared_fmt = reactable::colDef(name = "R\u00b2"),
          basis_risk_fmt = reactable::colDef(name = "Basis Risk"),
          vol_reduction_fmt = reactable::colDef(name = "Vol Red."),
          stress_delta_fmt = reactable::colDef(name = "Stress R2"),
          beta_sd_fmt = reactable::colDef(name = "Beta SD"),
          beta       = reactable::colDef(show = FALSE),
          r_squared  = reactable::colDef(show = FALSE),
          basis_risk = reactable::colDef(show = FALSE),
          vol_reduction_pct = reactable::colDef(show = FALSE),
          beta_sd = reactable::colDef(show = FALSE),
          stress_delta = reactable::colDef(show = FALSE)
        )
      )
    })

    output$per_tenor_table <- reactable::renderReactable({
      ptr <- page_data()$per_tenor_ratios %>%
        dplyr::filter(.data$market == focus_target()) %>%
        dplyr::arrange(.data$tenor)

      if (nrow(ptr) == 0L) {
        return(ea_empty_reactable())
      }

      reactable::reactable(
        ptr,
        pagination = FALSE,
        compact = TRUE,
        highlight = TRUE,
        theme = ea_reactable_theme(),
        columns = list(
          market = reactable::colDef(show = FALSE),
          tenor = reactable::colDef(name = "Tenor"),
          beta = reactable::colDef(name = "Beta", format = reactable::colFormat(digits = 2)),
          r_squared = reactable::colDef(name = "R2", format = reactable::colFormat(digits = 2))
        )
      )
    })

    output$rolling_hedge_table <- reactable::renderReactable({
      rb <- page_data()$rolling_beta %>%
        dplyr::filter(.data$market == focus_target()) %>%
        dplyr::arrange(.data$date)

      if (nrow(rb) == 0L) {
        return(ea_empty_reactable())
      }

      latest_beta <- utils::tail(rb$beta, 1)
      latest_r2 <- utils::tail(rb$r_squared, 1)
      avg_beta_30d <- mean(utils::tail(rb$beta, 30), na.rm = TRUE)
      beta_sd_30d <- stats::sd(utils::tail(rb$beta, 30), na.rm = TRUE)
      residual_band <- page_data()$residual_with_bands %>%
        dplyr::filter(.data$market == focus_target()) %>%
        dplyr::arrange(.data$date)
      ou_params <- page_data()$ou_fit %>%
        dplyr::filter(.data$market == focus_target())
      residual_z <- if (nrow(residual_band) > 0L) {
        latest_row <- utils::tail(residual_band, 1L)
        band_width <- latest_row$band_1_hi[[1]] - latest_row$ou_mu[[1]]
        if (is.finite(band_width) && band_width != 0) {
          (latest_row$residual[[1]] - latest_row$ou_mu[[1]]) / band_width
        } else {
          NA_real_
        }
      } else {
        NA_real_
      }

      display <- tibble::tribble(
        ~metric, ~value,
        "Target", focus_target(),
        "Latest Beta", scales::number(latest_beta, accuracy = 0.01),
        "Latest R2", scales::number(latest_r2, accuracy = 0.01),
        "30D Avg Beta", scales::number(avg_beta_30d, accuracy = 0.01),
        "30D Beta SD", if (is.finite(beta_sd_30d)) scales::number(beta_sd_30d, accuracy = 0.01) else "N/A",
        "Residual Z", if (is.finite(residual_z)) scales::number(residual_z, accuracy = 0.01) else "N/A",
        "Half-Life", if (nrow(ou_params) > 0L && is.finite(ou_params$half_life[[1]])) paste0(round(ou_params$half_life[[1]]), "d") else "N/A"
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

    # --- Row 1 right: Hedge Effectiveness grouped bar ---
    output$hedge_effectiveness <- plotly::renderPlotly({
      eff <- page_data()$hedge_effectiveness
      if (nrow(eff) == 0L) {
        return(ea_plotly_layout(plotly::plot_ly(), x_title = NULL, y_title = "Ann. Vol"))
      }
      fig <- plotly::plot_ly(data = eff, x = ~market) %>%
        plotly::add_bars(
          y = ~unhedged_vol,
          name = "Unhedged",
          marker = list(color = "#d36e70"),
          hovertemplate = "%{x}<br>Unhedged: %{y:.3f}<extra></extra>"
        ) %>%
        plotly::add_bars(
          y = ~hedged_vol,
          name = "Hedged",
          marker = list(color = "#4da3a3"),
          hovertemplate = "%{x}<br>Hedged: %{y:.3f}<extra></extra>"
        ) %>%
        plotly::layout(barmode = "group")
      ea_plotly_layout(fig, x_title = NULL, y_title = "Ann. Vol", hovermode = "closest")
    })

    # --- Row 2 left: Rolling Hedge Ratio ---
    output$rolling_hedge_ratio <- plotly::renderPlotly({
      rb <- page_data()$rolling_beta
      current_target <- focus_target()
      palette <- ea_market_palette()
      fig <- plotly::plot_ly()

      if (length(current_target) == 1L) {
        rb <- rb[rb$market == current_target, , drop = FALSE]
      }

      if (nrow(rb) == 0L) {
        return(ea_plotly_layout(fig, x_title = NULL, y_title = "Hedge Ratio"))
      }

      for (mkt in unique(rb$market)) {
        df <- rb[rb$market == mkt, , drop = FALSE]
        col <- if (!is.null(palette[[mkt]])) unname(palette[[mkt]]) else "#9aa6b2"

        fig <- fig %>%
          plotly::add_lines(
            x = df$date,
            y = df$beta,
            name = mkt,
            line = list(color = col, width = 1.5),
            hovertemplate = paste0("%{x|%d %b %Y}<br>Beta: %{y:.3f}<extra>", mkt, "</extra>")
          ) %>%
          plotly::add_lines(
            x = df$date,
            y = df$r_squared,
            name = paste0(mkt, " R2"),
            line = list(color = col, width = 1.2, dash = "dash"),
            yaxis = "y2",
            hovertemplate = paste0("%{x|%d %b %Y}<br>R2: %{y:.3f}<extra>", mkt, "</extra>")
          )
      }

      ea_plotly_layout(fig, x_title = NULL, y_title = "Hedge Ratio") %>%
        plotly::layout(
          yaxis2 = list(
            title = "R2",
            overlaying = "y",
            side = "right",
            color = "#9aa6b2",
            showgrid = FALSE
          )
        )
    })

    # --- Row 4 left: Hedge Ratio by Tenor ---
    output$per_tenor_ratios <- plotly::renderPlotly({
      ptr <- page_data()$per_tenor_ratios %>%
        dplyr::filter(.data$market == focus_target(), !is.na(.data$beta)) %>%
        dplyr::arrange(.data$tenor)
      fig <- plotly::plot_ly()

      if (nrow(ptr) == 0L) {
        return(ea_plotly_layout(fig, x_title = "Contract Tenor", y_title = "Hedge Ratio"))
      }

      col <- unname(ea_market_palette()[[focus_target()]]) %||% ea_accent_color()
      fig <- fig %>%
        plotly::add_lines(
          x = ptr$tenor,
          y = ptr$beta,
          name = "Beta",
          line = list(color = col, width = 2),
          hovertemplate = "Tenor %{x}<br>Beta: %{y:.3f}<extra></extra>"
        ) %>%
        plotly::add_lines(
          x = ptr$tenor,
          y = ptr$r_squared,
          name = "R2",
          line = list(color = "#5a85c8", width = 1.6, dash = "dash"),
          yaxis = "y2",
          hovertemplate = "Tenor %{x}<br>R2: %{y:.3f}<extra></extra>"
        )

      ea_plotly_layout(fig, x_title = "Contract Tenor", y_title = "Hedge Ratio") %>%
        plotly::layout(
          yaxis2 = list(
            title = "R2",
            overlaying = "y",
            side = "right",
            color = "#9aa6b2",
            showgrid = FALSE
          )
        )
    })

  })
}
