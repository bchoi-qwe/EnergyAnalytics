mod_options_greeks_ui <- function(id) {
  shiny::uiOutput(shiny::NS(id, "page"))
}

mod_options_greeks_server <- function(id, filters, data_timestamp) {
  shiny::moduleServer(id, function(input, output, session) {
    history_basis_choices <- c(
      "Trailing" = "trailing",
      "Full" = "full"
    )
    history_year_choices <- ea_history_year_choices()
    selected_context_basis <- shiny::reactive({
      current <- ea_coalesce(input$options_context_basis, "trailing")
      if (current %in% unname(history_basis_choices)) current else "trailing"
    })
    selected_context_years <- shiny::reactive({
      current <- suppressWarnings(as.integer(ea_coalesce(input$options_context_years, "5")[[1]]))
      if (!is.finite(current) || is.na(current) || current < 1L) {
        5L
      } else {
        min(current, 30L)
      }
    })
    selected_context <- shiny::reactive({
      if (identical(selected_context_basis(), "full")) "full" else paste0(selected_context_years(), "y")
    })
    page_data <- shiny::reactive(ea_calc_surface_greeks(filters(), history_context = selected_context()))
    view_choices <- c(
      "Gamma Surface" = "gamma",
      "Vega Surface" = "vega",
      "Vanna Surface" = "vanna",
      "Charm Surface" = "charm",
      "ATM Greeks" = "term",
      "Smile" = "smile",
      "History" = "history",
      "P&L" = "pnl"
    )
    available_markets <- shiny::reactive({
      mkts <- unique(page_data()$full_grid$market)
      mkts <- mkts[nzchar(mkts)]
      if (length(mkts) == 0L) {
        mkts <- ea_coalesce(filters()$commodities, character(0))
      }
      unique(mkts)
    })
    selected_market <- shiny::reactive({
      current_selection <- ea_coalesce(input$greeks_market, character(0))
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
      current <- ea_coalesce(input$options_view, "gamma")
      if (current %in% unname(view_choices)) current else "gamma"
    })
    selected_surface_metric <- shiny::reactive({
      switch(
        selected_view(),
        gamma = "gamma",
        vega = "vega",
        vanna = "vanna",
        charm = "charm",
        NULL
      )
    })
    surface_metric_summary <- function(metric_name) {
      chart_data <- page_data()$full_grid %>%
        dplyr::filter(.data$market == selected_market()) %>%
        dplyr::mutate(
          tenor_bucket = dplyr::case_when(
            .data$curve_point_num <= 1 ~ "Prompt",
            .data$curve_point_num <= 3 ~ "3M",
            .data$curve_point_num <= 6 ~ "6M",
            .data$curve_point_num <= 12 ~ "12M",
            TRUE ~ "Deferred"
          )
        )

      if (nrow(chart_data) == 0L) {
        return(tibble::tibble())
      }

      split(chart_data, chart_data$tenor_bucket) %>%
        purrr::imap_dfr(function(bucket_df, bucket_name) {
          metric_values <- bucket_df[[metric_name]]
          finite_idx <- which(is.finite(metric_values))

          if (length(finite_idx) == 0L) {
            return(tibble::tibble(
              tenor_bucket = bucket_name,
              exposure = NA_real_,
              peak = NA_real_,
              peak_moneyness = NA_real_
            ))
          }

          peak_idx <- finite_idx[[which.max(abs(metric_values[finite_idx]))]]

          tibble::tibble(
            tenor_bucket = bucket_name,
            exposure = sum(metric_values[finite_idx], na.rm = TRUE),
            peak = metric_values[[peak_idx]],
            peak_moneyness = bucket_df$moneyness[[peak_idx]]
          )
        })
    }
    selected_pnl_grid <- shiny::reactive({
      full_grid <- page_data()$full_grid %>%
        dplyr::filter(.data$market == selected_market())

      if (nrow(full_grid) == 0L) {
        return(tibble::tibble(spot_shock = numeric(), vol_shock = numeric(), pnl = numeric()))
      }

      prompt_num <- suppressWarnings(min(full_grid$curve_point_num, na.rm = TRUE))

      if (!is.finite(prompt_num)) {
        return(tibble::tibble(spot_shock = numeric(), vol_shock = numeric(), pnl = numeric()))
      }

      prompt_candidates <- full_grid %>%
        dplyr::filter(.data$curve_point_num == prompt_num)

      atm_row <- prompt_candidates %>%
        dplyr::filter(.data$option_type == "C") %>%
        dplyr::slice_min(abs(.data$moneyness - 1.0), n = 1L, with_ties = FALSE)

      if (nrow(atm_row) == 0L) {
        atm_row <- prompt_candidates %>%
          dplyr::slice_min(abs(.data$moneyness - 1.0), n = 1L, with_ties = FALSE)
      }

      if (nrow(atm_row) == 0L) {
        return(tibble::tibble(spot_shock = numeric(), vol_shock = numeric(), pnl = numeric()))
      }

      forward_0 <- atm_row$forward[[1]]
      strike_0 <- atm_row$strike[[1]]
      rate_0 <- atm_row$risk_free[[1]]
      tenor_years_0 <- atm_row$T_years[[1]]
      sigma_0 <- atm_row$iv[[1]]
      is_call <- stringr::str_to_upper(atm_row$option_type[[1]]) == "C"
      base_premium <- atm_row$premium[[1]]

      if (is.null(base_premium) || is.na(base_premium)) {
        base_premium <- black76_greeks_vectorised(
          F = forward_0,
          K = strike_0,
          r = rate_0,
          T = tenor_years_0,
          sigma = sigma_0,
          is_call = is_call
        )$premium
      }

      expand.grid(
        spot_shock = seq(-0.10, 0.10, by = 0.02),
        vol_shock = seq(-0.05, 0.05, by = 0.01)
      ) %>%
        tibble::as_tibble() %>%
        dplyr::mutate(
          forward_new = forward_0 * (1 + .data$spot_shock),
          sigma_new = pmax(sigma_0 + .data$vol_shock, 0.01),
          premium_new = black76_greeks_vectorised(
            F = .data$forward_new,
            K = rep(strike_0, dplyr::n()),
            r = rep(rate_0, dplyr::n()),
            T = rep(tenor_years_0, dplyr::n()),
            sigma = .data$sigma_new,
            is_call = rep(is_call, dplyr::n())
          )$premium,
          pnl = .data$premium_new - base_premium
        ) %>%
        dplyr::select(.data$spot_shock, .data$vol_shock, .data$pnl)
    })
    selected_view_spec <- shiny::reactive({
      switch(
        selected_view(),
        gamma = list(
          plot_title = "Gamma Surface",
          plot_output = session$ns("delta_gamma_surface"),
          table_title = "Surface Table",
          table_output = session$ns("surface_metric_table"),
          plot_height = "320px"
        ),
        vega = list(
          plot_title = "Vega Surface",
          plot_output = session$ns("vega_theta_surface"),
          table_title = "Surface Table",
          table_output = session$ns("surface_metric_table"),
          plot_height = "320px"
        ),
        vanna = list(
          plot_title = "Vanna Surface",
          plot_output = session$ns("greeks_vanna"),
          table_title = "Surface Table",
          table_output = session$ns("surface_metric_table"),
          plot_height = "320px"
        ),
        charm = list(
          plot_title = "Charm Surface",
          plot_output = session$ns("greeks_charm"),
          table_title = "Surface Table",
          table_output = session$ns("surface_metric_table"),
          plot_height = "320px"
        ),
        term = list(
          plot_title = "ATM Greeks",
          plot_output = session$ns("atm_greeks_term"),
          table_title = "Term Table",
          table_output = session$ns("term_greeks_table"),
          plot_height = "320px"
        ),
        smile = list(
          plot_title = "Smile",
          plot_output = session$ns("smile_evolution"),
          table_title = "Smile Table",
          table_output = session$ns("strike_profile_table"),
          plot_height = "320px"
        ),
        history = list(
          plot_title = "ATM IV History",
          plot_output = session$ns("atm_iv_history_chart"),
          table_title = "History Table",
          table_output = session$ns("surface_context_table"),
          plot_height = "320px"
        ),
        pnl = list(
          plot_title = "P&L Heatmap",
          plot_output = session$ns("pnl_heatmap"),
          table_title = "P&L Table",
          table_output = session$ns("pnl_summary_table"),
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
      years_visible_condition <- sprintf("input['%s'] !== 'full'", ns("options_context_basis"))

      if (length(current_filters$commodities) == 0L) {
        return(htmltools::tagList(
          ea_empty_state_card(
            title = "Select products to populate Greeks views",
            body = "Options Greeks computes Black-76 delta, gamma, vega, vanna, charm, speed, zomma, and more across the vol surface for the selected products.",
            hint = "All Greeks are calculated natively in C++ via the Rcpp pricing engine."
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
                input_id = ns("greeks_market"),
                label = "Market",
                choices = stats::setNames(available_markets(), ea_market_labels(available_markets())),
                selected = selected_choice
              )
            ),
            htmltools::tags$div(
              class = "ea-toolbar__field ea-toolbar__field--sm",
              ea_toolbar_select(
                input_id = ns("options_view"),
                label = "View",
                choices = view_choices,
                selected = selected_view_choice
              )
            ),
            htmltools::tags$div(
              class = "ea-toolbar__field ea-toolbar__field--sm",
              ea_toolbar_select(
                input_id = ns("options_context_basis"),
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
                  input_id = ns("options_context_years"),
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

    # ---- Delta-Gamma Surface heatmap ----
    output$delta_gamma_surface <- plotly::renderPlotly({
      fg <- page_data()$full_grid
      if (nrow(fg) == 0L) return(ea_plotly_layout(plotly::plot_ly(), x_title = "Moneyness", y_title = "Contract Month"))
      primary <- selected_market()
      df <- fg %>% dplyr::filter(.data$market == primary)
      if (nrow(df) == 0L) return(ea_plotly_layout(plotly::plot_ly(), x_title = "Moneyness", y_title = "Contract Month"))
      mat <- stats::xtabs(gamma ~ curve_point_num + moneyness, data = df)
      fig <- plotly::plot_ly(
        x = colnames(mat), y = rownames(mat), z = unclass(mat),
        type = "heatmap",
        colors = c("#17202b", "#4da3a3", "#d2a157"),
        hovertemplate = "Moneyness %{x}<br>Tenor M%{y}<br>Gamma: %{z:.6f}<extra>gamma</extra>"
      )
      ea_plotly_layout(fig, x_title = "Moneyness", y_title = "Contract Month", hovermode = "closest")
    })

    # ---- Vega-Theta Surface heatmap ----
    output$vega_theta_surface <- plotly::renderPlotly({
      fg <- page_data()$full_grid
      if (nrow(fg) == 0L) return(ea_plotly_layout(plotly::plot_ly(), x_title = "Moneyness", y_title = "Contract Month"))
      primary <- selected_market()
      df <- fg %>% dplyr::filter(.data$market == primary)
      if (nrow(df) == 0L) return(ea_plotly_layout(plotly::plot_ly(), x_title = "Moneyness", y_title = "Contract Month"))
      mat <- stats::xtabs(vega ~ curve_point_num + moneyness, data = df)
      fig <- plotly::plot_ly(
        x = colnames(mat), y = rownames(mat), z = unclass(mat),
        type = "heatmap",
        colors = c("#17202b", "#5a85c8", "#d36e70"),
        hovertemplate = "Moneyness %{x}<br>Tenor M%{y}<br>Vega: %{z:.4f}<extra>vega</extra>"
      )
      ea_plotly_layout(fig, x_title = "Moneyness", y_title = "Contract Month", hovermode = "closest")
    })

    # ---- ATM Greeks Term Structure ----
    output$atm_greeks_term <- plotly::renderPlotly({
      tg <- page_data()$term_greeks
      palette <- ea_market_palette()
      fig <- plotly::plot_ly()
      current_market <- selected_market()
      if (length(current_market) == 1L) {
        tg <- tg[tg$market == current_market, , drop = FALSE]
      }
      if (nrow(tg) == 0L) return(ea_plotly_layout(fig, x_title = "Contract Month", y_title = "Gamma"))
      for (mkt in unique(tg$market)) {
        df <- tg[tg$market == mkt, , drop = FALSE]
        col <- unname(palette[[mkt]])
        fig <- fig %>%
          plotly::add_trace(data = df, x = ~curve_point_num, y = ~gamma,
            type = "scatter", mode = "lines+markers",
            name = paste(unique(df$label), "Gamma"),
            line = list(color = col, width = 2), yaxis = "y",
            marker = list(color = col, size = 7),
            customdata = ~cbind(.data$atm_iv, .data$moneyness),
            hovertemplate = "M%{x}<br>Gamma: %{y:.6f}<br>ATM IV: %{customdata[0]:.1%}<br>Nearest mny: %{customdata[1]:.2f}<extra>%{fullData.name}</extra>") %>%
          plotly::add_trace(data = df, x = ~curve_point_num, y = ~vega,
            type = "scatter", mode = "lines+markers",
            name = paste(unique(df$label), "Vega"),
            line = list(color = col, width = 2, dash = "dash"), yaxis = "y2",
            marker = list(color = col, size = 6, symbol = "circle-open"),
            customdata = ~cbind(.data$atm_iv, .data$moneyness),
            hovertemplate = "M%{x}<br>Vega: %{y:.4f}<br>ATM IV: %{customdata[0]:.1%}<br>Nearest mny: %{customdata[1]:.2f}<extra>%{fullData.name}</extra>")
      }
      fig <- fig %>% plotly::layout(yaxis2 = list(title = "Vega", overlaying = "y", side = "right",
        color = "#9aa6b2", gridcolor = "rgba(148,163,184,0.04)", showgrid = FALSE))
      ea_plotly_layout(fig, x_title = "Contract Month", y_title = "Gamma")
    })

    # ---- Greeks Concentration table ----
    output$surface_metric_table <- reactable::renderReactable({
      metric_name <- selected_surface_metric()
      if (is.null(metric_name)) {
        return(ea_empty_reactable())
      }

      display <- surface_metric_summary(metric_name)

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
          tenor_bucket = reactable::colDef(name = "Tenor"),
          exposure = reactable::colDef(name = "Net", format = reactable::colFormat(digits = 4)),
          peak = reactable::colDef(name = "Peak", format = reactable::colFormat(digits = 4)),
          peak_moneyness = reactable::colDef(name = "Peak Mny", format = reactable::colFormat(digits = 2))
        )
      )
    })

    output$term_greeks_table <- reactable::renderReactable({
      tg <- page_data()$term_greeks %>%
        dplyr::filter(.data$market == selected_market()) %>%
        dplyr::left_join(
          page_data()$skew_ratio %>%
            dplyr::filter(.data$market == selected_market()) %>%
            dplyr::select(.data$curve_point_num, .data$skew_ratio),
          by = "curve_point_num"
        ) %>%
        dplyr::arrange(.data$curve_point_num)

      if (nrow(tg) == 0L) {
        return(ea_empty_reactable())
      }

      reactable::reactable(
        tg,
        pagination = FALSE,
        compact = TRUE,
        highlight = TRUE,
        theme = ea_reactable_theme(),
        columns = list(
          market = reactable::colDef(show = FALSE),
          label = reactable::colDef(show = FALSE),
          curve_point = reactable::colDef(name = "Tenor"),
          curve_point_num = reactable::colDef(show = FALSE),
          moneyness = reactable::colDef(name = "Mny", format = reactable::colFormat(digits = 2)),
          atm_gap = reactable::colDef(name = "Gap", format = reactable::colFormat(digits = 2)),
          atm_iv = reactable::colDef(name = "ATM IV", format = reactable::colFormat(percent = TRUE, digits = 1)),
          gamma = reactable::colDef(name = "Gamma", format = reactable::colFormat(digits = 6)),
          vega = reactable::colDef(name = "Vega", format = reactable::colFormat(digits = 3)),
          skew_ratio = reactable::colDef(name = "Skew", format = reactable::colFormat(digits = 2))
        )
      )
    })

    output$surface_context_table <- reactable::renderReactable({
      sc <- page_data()$surface_context %>%
        dplyr::filter(.data$market == selected_market())

      if (nrow(sc) == 0L) {
        return(ea_empty_reactable())
      }

      display <- tibble::tribble(
        ~metric, ~value,
        "Front ATM IV", if (is.finite(sc$atm_iv[[1]])) scales::percent(sc$atm_iv[[1]], accuracy = 0.1) else "N/A",
        "IV Percentile", if (is.finite(sc$iv_percentile[[1]])) scales::percent(sc$iv_percentile[[1]], accuracy = 1) else "N/A",
        "IV Z-Score", if (is.finite(sc$iv_zscore[[1]])) scales::number(sc$iv_zscore[[1]], accuracy = 0.01) else "N/A",
        "Front Skew", if (is.finite(sc$skew_ratio[[1]])) scales::number(sc$skew_ratio[[1]], accuracy = 0.01) else "N/A",
        "Skew Percentile", if (is.finite(sc$skew_percentile[[1]])) scales::percent(sc$skew_percentile[[1]], accuracy = 1) else "N/A",
        "Skew Z-Score", if (is.finite(sc$skew_zscore[[1]])) scales::number(sc$skew_zscore[[1]], accuracy = 0.01) else "N/A"
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

    output$strike_profile_table <- reactable::renderReactable({
      fg <- page_data()$full_grid %>%
        dplyr::filter(.data$market == selected_market())

      if (nrow(fg) == 0L) {
        return(ea_empty_reactable())
      }

      prompt_num <- min(fg$curve_point_num, na.rm = TRUE)
      display <- fg %>%
        dplyr::filter(.data$curve_point_num == prompt_num) %>%
        dplyr::transmute(
          option_type = dplyr::if_else(.data$option_type == "C", "Call", "Put"),
          moneyness = .data$moneyness,
          strike = .data$strike,
          iv = .data$iv,
          delta = .data$delta
        ) %>%
        dplyr::arrange(.data$option_type, .data$moneyness)

      reactable::reactable(
        display,
        pagination = FALSE,
        compact = TRUE,
        highlight = TRUE,
        theme = ea_reactable_theme(),
        columns = list(
          option_type = reactable::colDef(name = "Type"),
          moneyness = reactable::colDef(name = "Mny", format = reactable::colFormat(digits = 2)),
          strike = reactable::colDef(name = "Strike", format = reactable::colFormat(digits = 2)),
          iv = reactable::colDef(name = "IV", format = reactable::colFormat(percent = TRUE, digits = 1)),
          delta = reactable::colDef(name = "Delta", format = reactable::colFormat(digits = 3))
        )
      )
    })

    output$pnl_summary_table <- reactable::renderReactable({
      pg <- selected_pnl_grid()

      if (nrow(pg) == 0L) {
        return(ea_empty_reactable())
      }

      best_row <- pg %>% dplyr::slice_max(.data$pnl, n = 1, with_ties = FALSE)
      worst_row <- pg %>% dplyr::slice_min(.data$pnl, n = 1, with_ties = FALSE)
      display <- tibble::tribble(
        ~metric, ~value,
        "Scenario Mkt", ea_coalesce(selected_market(), "N/A"),
        "Best P&L", scales::number(best_row$pnl[[1]], accuracy = 0.0001),
        "Best Spot", scales::percent(best_row$spot_shock[[1]], accuracy = 1),
        "Best Vol", scales::percent(best_row$vol_shock[[1]], accuracy = 1),
        "Worst P&L", scales::number(worst_row$pnl[[1]], accuracy = 0.0001),
        "Worst Spot", scales::percent(worst_row$spot_shock[[1]], accuracy = 1),
        "Worst Vol", scales::percent(worst_row$vol_shock[[1]], accuracy = 1)
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

    # ---- Vanna heatmap ----
    output$greeks_vanna <- plotly::renderPlotly({
      chart_data <- page_data()$full_grid %>%
        dplyr::filter(.data$market == selected_market()) %>%
        dplyr::select(.data$curve_point_num, .data$moneyness, .data$vanna)
      if (nrow(chart_data) == 0L) return(ea_plotly_layout(plotly::plot_ly(), x_title = "Moneyness", y_title = "Contract Month"))
      vanna_matrix <- stats::xtabs(vanna ~ curve_point_num + moneyness, data = chart_data)

      fig <- plotly::plot_ly(
        x = colnames(vanna_matrix),
        y = rownames(vanna_matrix),
        z = unclass(vanna_matrix),
        type = "heatmap",
        colors = c("#17202b", "#4da3a3", "#d2a157"),
        hovertemplate = "Moneyness %{x}<br>Tenor M%{y}<br>Vanna: %{z:.4f}<extra>vanna</extra>"
      )

      ea_plotly_layout(fig, x_title = "Moneyness", y_title = "Contract Month", hovermode = "closest")
    })

    # ---- Charm heatmap ----
    output$greeks_charm <- plotly::renderPlotly({
      chart_data <- page_data()$full_grid %>%
        dplyr::filter(.data$market == selected_market()) %>%
        dplyr::select(.data$curve_point_num, .data$moneyness, .data$charm)
      if (nrow(chart_data) == 0L) return(ea_plotly_layout(plotly::plot_ly(), x_title = "Moneyness", y_title = "Contract Month"))
      charm_matrix <- stats::xtabs(charm ~ curve_point_num + moneyness, data = chart_data)

      fig <- plotly::plot_ly(
        x = colnames(charm_matrix),
        y = rownames(charm_matrix),
        z = unclass(charm_matrix),
        type = "heatmap",
        colors = c("#17202b", "#5a85c8", "#d36e70"),
        hovertemplate = "Moneyness %{x}<br>Tenor M%{y}<br>Charm: %{z:.6f}<extra>charm</extra>"
      )

      ea_plotly_layout(fig, x_title = "Moneyness", y_title = "Contract Month", hovermode = "closest")
    })

    # ---- Vol Smile Evolution ----
    output$smile_evolution <- plotly::renderPlotly({
      fg <- page_data()$full_grid
      if (nrow(fg) == 0L) return(ea_plotly_layout(plotly::plot_ly(), x_title = "Moneyness", y_title = "IV"))
      df <- fg %>% dplyr::filter(.data$market == selected_market())
      tenors <- sort(unique(df$curve_point_num))
      tenors <- utils::head(tenors, 4L)
      palette <- c("#4da3a3", "#5a85c8", "#d2a157", "#d36e70")
      sub_figs <- lapply(seq_along(tenors), function(i) {
        tn <- tenors[[i]]
        df_t <- df[df$curve_point_num == tn, , drop = FALSE]
        col <- palette[[i]]
        plotly::plot_ly(data = df_t, x = ~moneyness, y = ~iv, type = "scatter", mode = "lines",
          name = paste0("M", tn), line = list(color = col, width = 1.8),
          hovertemplate = paste0("M", tn, "<br>Moneyness: %{x:.0%}<br>IV: %{y:.1%}<extra></extra>"))
      })
      fig <- plotly::subplot(sub_figs, nrows = length(sub_figs), shareX = TRUE, titleX = FALSE)
      ea_plotly_layout(fig, x_title = "Moneyness", y_title = NULL)
    })

    output$atm_iv_history_chart <- plotly::renderPlotly({
      hist <- page_data()$atm_history %>%
        dplyr::filter(.data$market == selected_market())
      fig <- plotly::plot_ly()
      if (nrow(hist) == 0L) {
        return(ea_plotly_layout(fig, x_title = NULL, y_title = "ATM IV"))
      }

      front_hist <- hist %>%
        dplyr::filter(.data$curve_point_num == min(.data$curve_point_num, na.rm = TRUE)) %>%
        dplyr::arrange(.data$date)

      if (nrow(front_hist) == 0L) {
        return(ea_plotly_layout(fig, x_title = NULL, y_title = "ATM IV"))
      }

      fig <- fig %>%
        plotly::add_lines(
          data = front_hist,
          x = ~date,
          y = ~atm_iv,
          name = "ATM IV",
          line = list(color = "#4da3a3", width = 2),
          hovertemplate = "%{x|%d %b %Y}<br>ATM IV: %{y:.1%}<extra></extra>"
        )

      if (any(is.finite(front_hist$skew_ratio))) {
        fig <- fig %>%
          plotly::add_lines(
            data = front_hist,
            x = ~date,
            y = ~skew_ratio,
            name = "Skew",
            line = list(color = "#d2a157", width = 1.6, dash = "dash"),
            yaxis = "y2",
            hovertemplate = "%{x|%d %b %Y}<br>Skew: %{y:.2f}<extra></extra>"
          )
      }

      ea_plotly_layout(fig, x_title = NULL, y_title = "ATM IV") %>%
        plotly::layout(
          yaxis2 = list(
            title = "Skew",
            overlaying = "y",
            side = "right",
            color = "#9aa6b2",
            showgrid = FALSE
          )
        )
    })

    # ---- P&L Heatmap ----
    output$pnl_heatmap <- plotly::renderPlotly({
      pg <- selected_pnl_grid()
      if (nrow(pg) == 0L) return(ea_plotly_layout(plotly::plot_ly(), x_title = "Spot shock", y_title = "Vol shock"))
      mat <- stats::xtabs(pnl ~ vol_shock + spot_shock, data = pg)
      fig <- plotly::plot_ly(
        x = colnames(mat), y = rownames(mat), z = unclass(mat),
        type = "heatmap",
        colors = c("#b35c60", "#17202b", "#4da3a3"),
        hovertemplate = "Spot %{x:.0%}<br>Vol %{y:.0%}<br>P&L: %{z:.4f}<extra>pnl</extra>"
      )
      ea_plotly_layout(fig, x_title = "Spot shock", y_title = "Vol shock", hovermode = "closest")
    })

  })
}
