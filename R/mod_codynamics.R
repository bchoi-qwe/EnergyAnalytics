mod_codynamics_ui <- function(id) {
  shiny::uiOutput(shiny::NS(id, "page"))
}

mod_codynamics_server <- function(id, filters, data_timestamp) {
  shiny::moduleServer(id, function(input, output, session) {
    view_choices <- c(
      "Correlation" = "correlation",
      "Pair" = "pair",
      "Rates" = "rates"
    )
    history_basis_choices <- c(
      "Trailing" = "trailing",
      "Full" = "full"
    )
    history_year_choices <- ea_history_year_choices()

    empty_page_data <- function(error_message = NULL) {
      list(
        correlation_matrix = tibble::tibble(),
        correlation_timeseries = tibble::tibble(),
        spread_timeseries = tibble::tibble(),
        spread_zscore = tibble::tibble(),
        beta_matrix = tibble::tibble(),
        pca_decomposition = tibble::tibble(),
        treasury_curve_snapshot = tibble::tibble(),
        treasury_betas = tibble::tibble(),
        connectedness_score = NA_real_,
        correlation_breaks = tibble::tibble(),
        coint_residual = tibble::tibble(),
        rolling_beta_ts = tibble::tibble(),
        kpis = tibble::tibble(),
        notes = character(),
        assumptions = character(),
        .error = error_message
      )
    }

    selected_context_basis <- shiny::reactive({
      current <- ea_coalesce(input$cod_context_basis, "trailing")
      if (current %in% unname(history_basis_choices)) current else "trailing"
    })

    selected_context_years <- shiny::reactive({
      current <- suppressWarnings(as.integer(ea_coalesce(input$cod_context_years, "5")[[1]]))
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

    page_data <- shiny::reactive({
      tryCatch(
        {
          result <- ea_calc_codynamics(filters(), history_context = selected_context())
          result$.error <- NULL
          result
        },
        error = function(e) empty_page_data(conditionMessage(e))
      )
    })

    pair_choices <- shiny::reactive({
      choices <- unique(c(
        page_data()$correlation_timeseries$pair,
        page_data()$rolling_beta_ts$pair,
        page_data()$coint_residual$pair
      ))
      choices <- choices[!is.na(choices) & nzchar(choices)]
      sort(choices)
    })

    default_pair_choice <- shiny::reactive({
      choices <- pair_choices()
      if (length(choices) == 0L) {
        return(NA_character_)
      }

      cb <- page_data()$correlation_breaks
      if (nrow(cb) > 0L) {
        candidate <- cb %>%
          dplyr::mutate(abs_move = abs(.data$corr_delta)) %>%
          dplyr::arrange(dplyr::desc(.data$abs_move)) %>%
          dplyr::slice(1) %>%
          dplyr::pull(.data$pair)
        if (length(candidate) == 1L && candidate %in% choices) {
          return(candidate)
        }
      }

      choices[[1]]
    })

    selected_pair <- shiny::reactive({
      choices <- pair_choices()
      if (length(choices) == 0L) {
        return(NA_character_)
      }

      current <- ea_coalesce(input$focus_pair, default_pair_choice())
      if (current %in% choices) {
        current
      } else {
        default_pair_choice()
      }
    })

    selected_coint_pair <- shiny::reactive({
      cr <- page_data()$coint_residual
      if (is.null(cr) || nrow(cr) == 0L || !"pair" %in% names(cr)) {
        return(NA_character_)
      }

      available_pairs <- unique(cr$pair)
      focus_pair <- selected_pair()
      if (length(focus_pair) == 1L && focus_pair %in% available_pairs) {
        focus_pair
      } else {
        available_pairs[[1]]
      }
    })

    selected_view <- shiny::reactive({
      current <- ea_coalesce(input$cod_view, "correlation")
      if (current %in% unname(view_choices)) current else "correlation"
    })

    selected_view_spec <- shiny::reactive({
      switch(
        selected_view(),
        correlation = list(
          plot_title = "Correlation Matrix",
          plot_output = session$ns("corr_heatmap"),
          table_title = "Pair Table",
          table_output = session$ns("corr_breaks_table"),
          plot_height = "340px"
        ),
        pair = list(
          plot_title = "Pair Dynamics",
          plot_output = session$ns("rolling_corr"),
          table_title = "Pair Detail",
          table_output = session$ns("pair_detail_table"),
          plot_height = "340px"
        ),
        rates = list(
          plot_title = "Rate Betas",
          plot_output = session$ns("treasury_bars"),
          table_title = "UST Curve",
          table_output = session$ns("treasury_curve_table"),
          plot_height = "340px"
        )
      )
    })

    output$page <- shiny::renderUI({
      ns <- session$ns
      current_filters <- filters()
      has_cross_market <- length(unique(current_filters$commodities)) >= 2L
      pair_options <- pair_choices()
      pair_selected <- selected_pair()
      if (!(length(pair_selected) == 1L && pair_selected %in% pair_options)) {
        pair_selected <- if (!is.na(default_pair_choice())) default_pair_choice() else ""
      }
      selected_view_choice <- selected_view()
      selected_context_basis_choice <- selected_context_basis()
      selected_context_years_choice <- as.character(selected_context_years())
      pair_visible_condition <- sprintf("input['%s'] === 'pair'", ns("cod_view"))
      years_visible_condition <- sprintf("input['%s'] !== 'full'", ns("cod_context_basis"))

      if (length(unique(current_filters$commodities)) < 2L) {
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
        if (has_cross_market) {
          htmltools::tags$div(
            class = "ea-toolbar ea-toolbar--correlations",
            htmltools::tags$div(
              class = "ea-toolbar__row",
              shiny::conditionalPanel(
                condition = pair_visible_condition,
                htmltools::tags$div(
                  class = "ea-toolbar__field ea-toolbar__field--sm",
                  ea_toolbar_select(
                    input_id = ns("focus_pair"),
                    label = "Focus Pair",
                    choices = if (length(pair_options) > 0L) {
                      stats::setNames(pair_options, pair_options)
                    } else {
                      c("No pair available" = "")
                    },
                    selected = pair_selected
                  )
                )
              ),
              htmltools::tags$div(
                class = "ea-toolbar__field ea-toolbar__field--sm",
                ea_toolbar_select(
                  input_id = ns("cod_view"),
                  label = "View",
                  choices = view_choices,
                  selected = selected_view_choice
                )
              ),
              htmltools::tags$div(
                class = "ea-toolbar__field ea-toolbar__field--sm",
                ea_toolbar_select(
                  input_id = ns("cod_context_basis"),
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
                    input_id = ns("cod_context_years"),
                    label = "Years",
                    choices = history_year_choices,
                    selected = selected_context_years_choice
                  )
                )
              )
            )
          )
        },
        if (!has_cross_market) {
          ea_empty_state_card(
            title = "Add a second product",
            body = "Pair views need two products."
          )
        },
        if (has_cross_market) {
          shiny::uiOutput(ns("view_panel"))
        }
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

    output$corr_breaks_table <- reactable::renderReactable({
      cb <- page_data()$correlation_breaks

      if (nrow(cb) == 0L) {
        return(ea_empty_reactable())
      }

      tbl <- cb %>%
        dplyr::mutate(
          corr_pct = dplyr::if_else(
            is.finite(.data$corr_percentile),
            scales::percent(.data$corr_percentile, accuracy = 1),
            "N/A"
          ),
          abs_move = abs(.data$corr_delta)
        ) %>%
        dplyr::arrange(dplyr::desc(.data$abs_move)) %>%
        dplyr::transmute(
          pair = .data$pair,
          corr = .data$current_corr,
          delta = .data$corr_delta,
          percentile = .data$corr_pct,
          beta = .data$beta,
          r_squared = .data$r_squared
        )

      reactable::reactable(
        tbl,
        theme = ea_reactable_theme(),
        compact = TRUE,
        highlight = TRUE,
        columns = list(
          pair = reactable::colDef(name = "Pair", minWidth = 140),
          corr = reactable::colDef(
            name = "Corr",
            minWidth = 90,
            format = reactable::colFormat(digits = 2),
            style = function(value) {
              list(color = if (is.na(value)) "#9aa6b2" else if (value >= 0) "#4da3a3" else "#d36e70")
            }
          ),
          delta = reactable::colDef(
            name = "20D Chg",
            minWidth = 90,
            format = reactable::colFormat(digits = 2)
          ),
          percentile = reactable::colDef(name = "Pct"),
          beta = reactable::colDef(name = "Beta", format = reactable::colFormat(digits = 2)),
          r_squared = reactable::colDef(name = "R2", format = reactable::colFormat(digits = 2))
        )
      )
    })

    output$pair_detail_table <- reactable::renderReactable({
      pair_name <- selected_pair()

      if (is.na(pair_name) || !nzchar(pair_name)) {
        return(ea_empty_reactable())
      }

      pair_row <- page_data()$correlation_breaks %>%
        dplyr::filter(.data$pair == pair_name)
      resid_ts <- page_data()$coint_residual %>%
        dplyr::filter(.data$pair == selected_coint_pair()) %>%
        dplyr::arrange(.data$date)
      latest_resid <- if (nrow(resid_ts) > 0L) utils::tail(resid_ts$residual, 1L) else NA_real_
      resid_z <- if (nrow(pair_row) > 0L) pair_row$residual_z[[1]] else NA_real_

      display <- tibble::tribble(
        ~metric, ~value,
        "Pair", pair_name,
        "Latest Corr", if (nrow(pair_row) > 0L && is.finite(pair_row$current_corr[[1]])) scales::number(pair_row$current_corr[[1]], accuracy = 0.01) else "N/A",
        "20D Corr Chg", if (nrow(pair_row) > 0L && is.finite(pair_row$corr_delta[[1]])) scales::number(pair_row$corr_delta[[1]], accuracy = 0.01) else "N/A",
        "Corr Percentile", if (nrow(pair_row) > 0L && is.finite(pair_row$corr_percentile[[1]])) scales::percent(pair_row$corr_percentile[[1]], accuracy = 1) else "N/A",
        "Context Beta", if (nrow(pair_row) > 0L && is.finite(pair_row$beta[[1]])) scales::number(pair_row$beta[[1]], accuracy = 0.01) else "N/A",
        "Context R2", if (nrow(pair_row) > 0L && is.finite(pair_row$r_squared[[1]])) scales::number(pair_row$r_squared[[1]], accuracy = 0.01) else "N/A",
        "Residual Z", if (is.finite(resid_z)) scales::number(resid_z, accuracy = 0.01) else "N/A"
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

    # ---- Rolling Correlation ----
    output$rolling_corr <- plotly::renderPlotly({
      ct <- page_data()$correlation_timeseries
      bt <- page_data()$rolling_beta_ts
      fig <- plotly::plot_ly()
      pair_name <- selected_pair()

      if (nrow(ct) == 0L || is.na(pair_name)) {
        return(ea_plotly_layout(fig, x_title = NULL, y_title = "Correlation"))
      }

      df <- ct[ct$pair == pair_name, , drop = FALSE]
      beta_df <- bt[bt$pair == pair_name, , drop = FALSE]
      if (nrow(df) == 0L) {
        return(ea_plotly_layout(fig, x_title = NULL, y_title = "Correlation"))
      }

      fig <- fig %>%
        plotly::add_ribbons(
          data = df,
          x = ~date,
          ymin = ~ci_lo,
          ymax = ~ci_hi,
          name = "Confidence band",
          line = list(color = "transparent"),
          fillcolor = "rgba(77,163,163,0.18)",
          hoverinfo = "skip"
        ) %>%
        plotly::add_lines(
          data = df,
          x = ~date,
          y = ~correlation,
          name = pair_name,
          line = list(color = "#4da3a3", width = 2.2),
          hovertemplate = "%{x|%d %b %Y}<br>Correlation: %{y:.2f}<extra></extra>"
        )

      if (nrow(beta_df) > 0L) {
        fig <- fig %>%
          plotly::add_lines(
            data = beta_df,
            x = ~date,
            y = ~beta,
            name = "Beta",
            line = list(color = "#5a85c8", width = 1.8, dash = "dash"),
            yaxis = "y2",
            hovertemplate = "%{x|%d %b %Y}<br>Beta: %{y:.2f}<extra></extra>"
          )
      }

      fig <- fig %>%
        plotly::layout(shapes = list(
          list(
            type = "line",
            x0 = min(df$date),
            x1 = max(df$date),
            y0 = 0,
            y1 = 0,
            line = list(color = "#7f8b99", width = 1, dash = "dot")
          )
        ))

      ea_plotly_layout(fig, x_title = NULL, y_title = "Correlation") %>%
        plotly::layout(
          yaxis = list(range = c(-1, 1)),
          yaxis2 = list(
            title = "Beta",
            overlaying = "y",
            side = "right",
            color = "#9aa6b2",
            showgrid = FALSE
          )
        )
    })

    # ---- Correlation Matrix Heatmap ----
    output$corr_heatmap <- plotly::renderPlotly({
      cm <- page_data()$correlation_matrix
      if (nrow(cm) == 0L) {
        return(ea_plotly_layout(plotly::plot_ly(), x_title = NULL, y_title = NULL))
      }

      market_order <- intersect(filters()$commodities, unique(c(cm$market_x, cm$market_y)))
      if (length(market_order) == 0L) {
        market_order <- sort(unique(c(cm$market_x, cm$market_y)))
      }

      cm <- cm %>%
        dplyr::mutate(
          market_x = factor(.data$market_x, levels = market_order),
          market_y = factor(.data$market_y, levels = rev(market_order))
        )

      dep_matrix <- stats::xtabs(correlation ~ market_y + market_x, data = cm)

      fig <- plotly::plot_ly(
        x = colnames(dep_matrix),
        y = rownames(dep_matrix),
        z = unclass(dep_matrix),
        type = "heatmap",
        colors = c("#b35c60", "#17202b", "#4da3a3"),
        zmin = -1,
        zmax = 1,
        colorbar = list(title = "Corr"),
        hovertemplate = "%{y} vs %{x}<br>%{z:.2f}<extra>correlation</extra>"
      )

      ea_plotly_layout(fig, x_title = NULL, y_title = NULL, hovermode = "closest")
    })

    output$treasury_curve_table <- reactable::renderReactable({
      tc <- page_data()$treasury_curve_snapshot %>%
        dplyr::arrange(.data$tenor_years)

      if (nrow(tc) == 0L) {
        return(ea_empty_reactable())
      }

      display <- tc %>%
        dplyr::transmute(
          tenor = .data$tenor,
          yield = scales::number(.data$yield, accuracy = 0.01, suffix = "%"),
          change_bps = dplyr::if_else(
            is.finite(.data$change_bps),
            sprintf("%+.1f bp", .data$change_bps),
            "N/A"
          )
        )

      reactable::reactable(
        display,
        pagination = FALSE,
        compact = TRUE,
        highlight = TRUE,
        theme = ea_reactable_theme(),
        columns = list(
          tenor = reactable::colDef(name = "Tenor"),
          yield = reactable::colDef(name = "Yield"),
          change_bps = reactable::colDef(name = "1D Change")
        )
      )
    })

    # ---- Treasury Factor Sensitivity ----
    output$treasury_bars <- plotly::renderPlotly({
      tb <- page_data()$treasury_betas
      factor_colors <- c(
        "Level" = "#4da3a3",
        "Slope" = "#5a85c8",
        "Curvature" = "#d2a157"
      )

      fig <- plotly::plot_ly()

      if (nrow(tb) == 0L) {
        return(ea_plotly_layout(fig, x_title = NULL, y_title = "Beta"))
      }

      for (fac in c("Level", "Slope", "Curvature")) {
        df_f <- tb[tb$factor == fac, , drop = FALSE]
        if (nrow(df_f) == 0L) next
        fig <- fig %>%
          plotly::add_bars(
            data = df_f,
            x = ~market,
            y = ~beta,
            name = fac,
            marker = list(color = factor_colors[[fac]]),
            hovertemplate = paste0("%{x}<br>", fac, " beta: %{y:.4f}<extra></extra>")
          )
      }

      fig <- fig %>%
        plotly::layout(barmode = "group")

      ea_plotly_layout(fig, x_title = NULL, y_title = "Beta")
    })

  })
}
