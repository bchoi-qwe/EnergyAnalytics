mod_hedging_ui <- function(id) {
  shiny::uiOutput(shiny::NS(id, "page"))
}

mod_hedging_server <- function(id, filters, data_timestamp) {
  shiny::moduleServer(id, function(input, output, session) {
    page_data <- shiny::reactive(ea_calc_hedging(filters()))

    mod_kpi_strip_server("kpi_strip", kpis = shiny::reactive(page_data()$kpis))
    mod_footer_notes_server(
      "footer_notes",
      notes = shiny::reactive(list(
        notes = page_data()$notes,
        assumptions = page_data()$assumptions,
        timestamp = data_timestamp()
      )),
      title = "Cross-hedge notes"
    )

    output$page <- shiny::renderUI({
      ns <- session$ns
      current_filters <- filters()

      if (length(current_filters$commodities) == 0L) {
        return(htmltools::tagList(
          ea_selected_filters_ribbon(current_filters, data_timestamp()),
          ea_empty_state_card(
            title = "Select products to populate hedge analytics",
            body = "Cross-Hedge focuses on hedge ratio term structure, hedge history, and effectiveness versus the selected benchmark.",
            hint = "The benchmark selector acts as the proxy hedge leg."
          ),
          mod_footer_notes_ui(ns("footer_notes"))
        ))
      }

      htmltools::tagList(
        ea_selected_filters_ribbon(current_filters, data_timestamp()),
        mod_kpi_strip_ui(ns("kpi_strip")),
        # Row 1: Cross-Hedge Ranking Table | Hedge Effectiveness Comparison
        bslib::layout_columns(
          col_widths = c(7, 5),
          ea_table_card(
            title = "Cross-Hedge Ranking",
            subtitle = "Pairwise OLS beta and R-squared, sorted by effectiveness.",
            output_id = ns("cross_hedge_table")
          ),
          ea_plotly_card(
            title = "Hedge Effectiveness",
            subtitle = "Unhedged vs hedged annualised volatility by market.",
            output_id = ns("hedge_effectiveness"),
            height = "240px"
          )
        ),
        # Row 2: Rolling Hedge Ratio | Rolling R-squared
        bslib::layout_columns(
          col_widths = c(6, 6),
          ea_plotly_card(
            title = "Rolling Hedge Ratio",
            subtitle = "Sliding window OLS beta with approximate SE band.",
            output_id = ns("rolling_hedge_ratio"),
            height = "240px"
          ),
          ea_plotly_card(
            title = "Rolling R-Squared",
            subtitle = "Hedge quality over time by market.",
            output_id = ns("rolling_r_squared"),
            height = "240px"
          )
        ),
        # Row 3: Hedge Residual with OU bands | OU Parameters Summary
        bslib::layout_columns(
          col_widths = c(8, 4),
          ea_plotly_card(
            title = "Hedge Residual with Mean-Reversion Bands",
            subtitle = "Cumulative OLS residual with OU \u00b11\u03c3 and \u00b12\u03c3 bands.",
            output_id = ns("residual_ou_bands"),
            height = "260px"
          ),
          ea_table_card(
            title = "OU Parameters",
            subtitle = "Mean-reversion speed, level, and half-life.",
            output_id = ns("ou_params_table")
          )
        ),
        # Row 4: Hedge Ratio by Tenor | Stress-Period Breakdown
        bslib::layout_columns(
          col_widths = c(6, 6),
          ea_plotly_card(
            title = "Hedge Ratio by Tenor",
            subtitle = "OLS beta at each contract tenor.",
            output_id = ns("per_tenor_ratios"),
            height = "240px"
          ),
          ea_plotly_card(
            title = "Stress-Period R-Squared",
            subtitle = "Normal vs high-vol regime hedge quality.",
            output_id = ns("stress_period"),
            height = "240px"
          )
        ),
        # Row 5: Hedging Cost Waterfall
        bslib::layout_columns(
          col_widths = 12,
          ea_plotly_card(
            title = "Hedging Cost Breakdown",
            subtitle = "Roll cost (M1-M2 annualised) and residual basis risk per target.",
            output_id = ns("hedge_cost_waterfall"),
            height = "220px"
          )
        ),
        mod_footer_notes_ui(ns("footer_notes"))
      )
    })

    # --- Row 1 left: Cross-Hedge Ranking Table ---
    output$cross_hedge_table <- reactable::renderReactable({
      mat <- page_data()$cross_hedge_matrix |>
        dplyr::filter(.data$market_x != .data$market_y) |>
        dplyr::arrange(dplyr::desc(.data$r_squared)) |>
        dplyr::mutate(
          basis_risk = 1 - .data$r_squared,
          r_squared_fmt = scales::percent(.data$r_squared, accuracy = 0.1),
          basis_risk_fmt = scales::number(.data$basis_risk, accuracy = 0.01),
          beta_fmt = scales::number(.data$beta, accuracy = 0.01)
        )

      reactable::reactable(
        mat,
        theme = ea_reactable_theme(),
        sortable = TRUE,
        defaultSorted = list(r_squared = "desc"),
        columns = list(
          market_x   = reactable::colDef(name = "Market"),
          market_y   = reactable::colDef(name = "Proxy"),
          beta_fmt   = reactable::colDef(name = "Beta"),
          r_squared_fmt = reactable::colDef(name = "R\u00b2"),
          basis_risk_fmt = reactable::colDef(name = "Basis Risk"),
          beta       = reactable::colDef(show = FALSE),
          r_squared  = reactable::colDef(show = FALSE),
          basis_risk = reactable::colDef(show = FALSE)
        )
      )
    })

    # --- Row 1 right: Hedge Effectiveness grouped bar ---
    output$hedge_effectiveness <- plotly::renderPlotly({
      eff <- page_data()$hedge_effectiveness
      fig <- plotly::plot_ly(data = eff, x = ~market, barmode = "group") |>
        plotly::add_bars(
          y = ~unhedged_vol,
          name = "Unhedged",
          marker = list(color = "#d36e70"),
          hovertemplate = "%{x}<br>Unhedged: %{y:.3f}<extra></extra>"
        ) |>
        plotly::add_bars(
          y = ~hedged_vol,
          name = "Hedged",
          marker = list(color = "#4da3a3"),
          hovertemplate = "%{x}<br>Hedged: %{y:.3f}<extra></extra>"
        )
      ea_plotly_layout(fig, x_title = NULL, y_title = "Ann. Vol", hovermode = "closest")
    })

    # --- Row 2 left: Rolling Hedge Ratio with SE band ---
    output$rolling_hedge_ratio <- plotly::renderPlotly({
      rb <- page_data()$rolling_beta
      palette <- ea_market_palette()
      fig <- plotly::plot_ly()

      if (nrow(rb) == 0L) {
        return(ea_plotly_layout(fig, x_title = NULL, y_title = "Hedge Ratio"))
      }

      for (mkt in unique(rb$market)) {
        df <- rb[rb$market == mkt, , drop = FALSE]
        col <- if (!is.null(palette[[mkt]])) unname(palette[[mkt]]) else "#9aa6b2"

        # Approximate SE band as +/- 10% of abs(beta)
        band_hi <- df$beta + 0.1 * abs(df$beta)
        band_lo <- df$beta - 0.1 * abs(df$beta)

        fig <- fig |>
          plotly::add_ribbons(
            x = df$date,
            ymin = band_lo,
            ymax = band_hi,
            fillcolor = {
              hex <- sub("^#", "", col)
              r <- strtoi(substr(hex, 1, 2), 16L)
              g <- strtoi(substr(hex, 3, 4), 16L)
              b <- strtoi(substr(hex, 5, 6), 16L)
              paste0("rgba(", r, ",", g, ",", b, ",0.12)")
            },
            line = list(width = 0),
            showlegend = FALSE,
            hoverinfo = "skip",
            name = paste0(mkt, " band")
          ) |>
          plotly::add_lines(
            x = df$date,
            y = df$beta,
            name = mkt,
            line = list(color = col, width = 1.5),
            hovertemplate = paste0("%{x|%d %b %Y}<br>Beta: %{y:.3f}<extra>", mkt, "</extra>")
          )
      }

      ea_plotly_layout(fig, x_title = NULL, y_title = "Hedge Ratio")
    })

    # --- Row 2 right: Rolling R-squared ---
    output$rolling_r_squared <- plotly::renderPlotly({
      rb <- page_data()$rolling_beta
      palette <- ea_market_palette()
      fig <- plotly::plot_ly()

      if (nrow(rb) == 0L) {
        return(ea_plotly_layout(fig, x_title = NULL, y_title = "R\u00b2"))
      }

      for (mkt in unique(rb$market)) {
        df <- rb[rb$market == mkt, , drop = FALSE]
        col <- if (!is.null(palette[[mkt]])) unname(palette[[mkt]]) else "#9aa6b2"

        fig <- fig |>
          plotly::add_lines(
            x = df$date,
            y = df$r_squared,
            name = mkt,
            line = list(color = col, width = 1.5),
            hovertemplate = paste0("%{x|%d %b %Y}<br>R\u00b2: %{y:.3f}<extra>", mkt, "</extra>")
          )
      }

      ea_plotly_layout(fig, x_title = NULL, y_title = "R\u00b2")
    })

    # --- Row 3 left: Hedge Residual with OU bands ---
    output$residual_ou_bands <- plotly::renderPlotly({
      rwb <- page_data()$residual_with_bands
      targets_in_data <- unique(rwb$market)
      palette <- ea_market_palette()

      if (length(targets_in_data) == 0) {
        return(ea_plotly_layout(plotly::plot_ly(), x_title = NULL, y_title = "Cumulative Residual"))
      }

      subplots <- lapply(targets_in_data, function(mkt) {
        df <- rwb[rwb$market == mkt, , drop = FALSE]
        col <- if (!is.null(palette[[mkt]])) unname(palette[[mkt]]) else "#9aa6b2"

        p <- plotly::plot_ly()

        # band_2 (faint)
        if (!all(is.na(df$band_2_lo))) {
          p <- p |>
            plotly::add_ribbons(
              x = df$date, ymin = df$band_2_lo, ymax = df$band_2_hi,
              fillcolor = "rgba(148,163,184,0.06)",
              line = list(width = 0), showlegend = FALSE, hoverinfo = "skip",
              name = paste0(mkt, " 2\u03c3 band")
            )
        }

        # band_1 (stronger)
        if (!all(is.na(df$band_1_lo))) {
          p <- p |>
            plotly::add_ribbons(
              x = df$date, ymin = df$band_1_lo, ymax = df$band_1_hi,
              fillcolor = "rgba(148,163,184,0.12)",
              line = list(width = 0), showlegend = FALSE, hoverinfo = "skip",
              name = paste0(mkt, " 1\u03c3 band")
            )
        }

        # OU mean line
        if (!all(is.na(df$ou_mu))) {
          p <- p |>
            plotly::add_lines(
              x = df$date, y = rep(df$ou_mu[1], nrow(df)),
              line = list(color = "#d2a157", dash = "dash", width = 1),
              showlegend = FALSE, hoverinfo = "skip",
              name = paste0(mkt, " OU \u03bc")
            )
        }

        # residual line
        p <- p |>
          plotly::add_lines(
            x = df$date, y = df$residual,
            name = mkt,
            line = list(color = col, width = 1.5),
            hovertemplate = paste0("%{x|%d %b %Y}<br>%{y:.4f}<extra>", mkt, "</extra>")
          )

        p
      })

      if (length(subplots) == 1) {
        ea_plotly_layout(subplots[[1]], x_title = NULL, y_title = "Cum. Residual")
      } else {
        fig <- plotly::subplot(subplots, nrows = length(subplots), shareX = TRUE, titleY = TRUE)
        ea_plotly_layout(fig, x_title = NULL, y_title = "Cum. Residual")
      }
    })

    # --- Row 3 right: OU Parameters Summary ---
    output$ou_params_table <- reactable::renderReactable({
      ou <- page_data()$ou_fit |>
        dplyr::mutate(
          half_life_fmt = dplyr::if_else(
            !is.na(.data$half_life),
            paste0(round(.data$half_life), "d"),
            "N/A"
          ),
          theta_fmt = scales::number(.data$theta, accuracy = 0.0001),
          mu_fmt = scales::number(.data$mu, accuracy = 0.0001),
          sigma_fmt = scales::number(.data$sigma, accuracy = 0.0001)
        )

      reactable::reactable(
        ou,
        theme = ea_reactable_theme(),
        columns = list(
          market     = reactable::colDef(name = "Market"),
          theta_fmt  = reactable::colDef(name = "Theta"),
          mu_fmt     = reactable::colDef(name = "Mu"),
          sigma_fmt  = reactable::colDef(name = "Sigma"),
          half_life_fmt = reactable::colDef(
            name = "Half-Life",
            style = function(value) {
              hl <- suppressWarnings(as.numeric(gsub("d", "", value)))
              if (!is.na(hl) && hl < 30) {
                list(color = "#4da3a3", fontWeight = "600")
              } else if (!is.na(hl) && hl > 60) {
                list(color = "#d36e70")
              } else {
                list()
              }
            }
          ),
          theta      = reactable::colDef(show = FALSE),
          mu         = reactable::colDef(show = FALSE),
          sigma      = reactable::colDef(show = FALSE),
          half_life  = reactable::colDef(show = FALSE)
        )
      )
    })

    # --- Row 4 left: Hedge Ratio by Tenor ---
    output$per_tenor_ratios <- plotly::renderPlotly({
      ptr <- page_data()$per_tenor_ratios |>
        dplyr::filter(!is.na(.data$beta))
      palette <- ea_market_palette()
      fig <- plotly::plot_ly()

      if (nrow(ptr) == 0L) {
        return(ea_plotly_layout(fig, x_title = "Contract Tenor", y_title = "Hedge Ratio"))
      }

      for (mkt in unique(ptr$market)) {
        df <- ptr[ptr$market == mkt, , drop = FALSE] |> dplyr::arrange(.data$tenor)
        col <- if (!is.null(palette[[mkt]])) unname(palette[[mkt]]) else "#9aa6b2"

        fig <- fig |>
          plotly::add_lines(
            x = df$tenor,
            y = df$beta,
            name = mkt,
            line = list(color = col, width = 1.5),
            hovertemplate = paste0("Tenor %{x}<br>Beta: %{y:.3f}<extra>", mkt, "</extra>")
          )
      }

      ea_plotly_layout(fig, x_title = "Contract Tenor", y_title = "Hedge Ratio")
    })

    # --- Row 4 right: Stress-Period Breakdown ---
    output$stress_period <- plotly::renderPlotly({
      sp <- page_data()$stress_period_comparison |>
        dplyr::filter(!is.na(.data$r2_normal) | !is.na(.data$r2_stress))
      palette <- ea_market_palette()

      fig <- plotly::plot_ly() |>
        plotly::add_lines(
          x = c(0, 1), y = c(0, 1),
          line = list(color = "#9aa6b2", dash = "dash", width = 1),
          showlegend = FALSE,
          hoverinfo = "skip",
          name = "Diagonal"
        )

      if (nrow(sp) > 0) {
        for (mkt in unique(sp$market)) {
          df <- sp[sp$market == mkt, , drop = FALSE]
          col <- if (!is.null(palette[[mkt]])) unname(palette[[mkt]]) else "#9aa6b2"

          fig <- fig |>
            plotly::add_markers(
              x = df$r2_normal,
              y = df$r2_stress,
              name = mkt,
              text = mkt,
              textposition = "top right",
              mode = "markers+text",
              marker = list(color = col, size = 10),
              hovertemplate = paste0(
                mkt, "<br>Normal R\u00b2: %{x:.3f}<br>Stress R\u00b2: %{y:.3f}<extra></extra>"
              )
            )
        }
      }

      ea_plotly_layout(
        fig,
        x_title = "R\u00b2 (Normal)",
        y_title = "R\u00b2 (Stress)",
        hovermode = "closest"
      )
    })

    # --- Row 5: Hedging Cost Waterfall ---
    output$hedge_cost_waterfall <- plotly::renderPlotly({
      hc <- page_data()$hedge_cost

      fig <- plotly::plot_ly(data = hc, x = ~market, barmode = "stack") |>
        plotly::add_bars(
          y = ~roll_cost,
          name = "Roll Cost",
          marker = list(color = "#5a85c8"),
          hovertemplate = "%{x}<br>Roll Cost: %{y:.3f}<extra></extra>"
        ) |>
        plotly::add_bars(
          y = ~basis_risk_cost,
          name = "Basis Risk",
          marker = list(color = "#d36e70"),
          hovertemplate = "%{x}<br>Basis Risk: %{y:.3f}<extra></extra>"
        )

      ea_plotly_layout(fig, x_title = NULL, y_title = "Cost", hovermode = "closest")
    })
  })
}
