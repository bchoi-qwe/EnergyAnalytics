mod_forward_curves_ui <- function(id) {
  shiny::uiOutput(shiny::NS(id, "page"))
}

mod_forward_curves_server <- function(id, filters, data_timestamp) {
  shiny::moduleServer(id, function(input, output, session) {
    page_data <- shiny::reactive(ea_mock_forward_curve_data(filters()))

    mod_footer_notes_server(
      "footer_notes",
      notes = shiny::reactive(list(
        notes = page_data()$notes,
        assumptions = page_data()$assumptions,
        timestamp = data_timestamp()
      )),
      title = "Term structure notes"
    )

    output$page <- shiny::renderUI({
      ns <- session$ns
      current_filters <- filters()
      catalog <- ea_market_catalog()

      if (length(current_filters$commodities) == 0L) {
        return(htmltools::tagList(
          ea_selected_filters_ribbon(current_filters, data_timestamp()),
          ea_empty_state_card(
            title = "Select products to populate term structure",
            body = "Term Structure focuses on curve shape, historical curve replay, and spread structure across the selected products.",
            hint = "Use the product multi-select, then refine curve date and tenor treatment with local controls."
          ),
          mod_footer_notes_ui(ns("footer_notes"))
        ))
      }

      htmltools::tagList(
        ea_selected_filters_ribbon(current_filters, data_timestamp()),
        ea_standard_card(
          title = "Term Structure Controls",
          subtitle = "Benchmark curve, tenor treatment, and observation date.",
          full_screen = FALSE,
          class = "ea-card--controls",
          bslib::layout_columns(
            col_widths = c(4, 4, 4),
            shinyWidgets::pickerInput(
              inputId = ns("benchmark_overlay"),
              label = "Benchmark curve",
              choices = c("None" = "none", stats::setNames(catalog$market, catalog$label)),
              selected = "none"
            ),
            shinyWidgets::pickerInput(
              inputId = ns("tenor_granularity"),
              label = "Tenor set",
              choices = c("Monthly" = "Monthly", "Quarterly" = "Quarterly", "Seasonal Strip" = "Seasonal"),
              selected = "Monthly"
            ),
            shinyWidgets::pickerInput(
              inputId = ns("snapshot_date"),
              label = "Curve date",
              choices = c("Latest", "1M Ago", "3M Ago"),
              selected = "Latest"
            )
          )
        ),
        bslib::layout_columns(
          col_widths = c(8, 4),
          ea_plotly_card(
            title = "Term Structure",
            subtitle = "Forward curve by listed contract month.",
            output_id = ns("curve_snapshot"),
            height = "380px"
          ),
          ea_plotly_card(
            title = "Historical Curves",
            subtitle = "Historical analysis of the lead product across prior curve dates.",
            output_id = ns("curve_history"),
            height = "380px"
          )
        ),
        bslib::layout_columns(
          col_widths = c(7, 5),
          ea_plotly_card(
            title = "Curve Shift Heatmap",
            subtitle = "Contract-month change through time.",
            output_id = ns("curve_heatmap"),
            height = "330px"
          ),
          ea_plotly_card(
            title = "Calendar / Intercommodity Spreads",
            subtitle = "Intracommodity and intercommodity spread monitor.",
            output_id = ns("spread_structure"),
            height = "330px"
          )
        ),
        mod_footer_notes_ui(ns("footer_notes"))
      )
    })

    output$curve_snapshot <- plotly::renderPlotly({
      chart_data <- page_data()$curve_snapshot
      granularity <- ea_coalesce(input$tenor_granularity, "Monthly")
      overlay <- ea_coalesce(input$benchmark_overlay, "none")
      palette <- ea_market_palette()

      keep_tenors <- switch(
        granularity,
        Quarterly = chart_data$tenor %% 3 == 0,
        Seasonal = chart_data$tenor %% 6 == 0,
        rep(TRUE, nrow(chart_data))
      )

      chart_data <- chart_data[keep_tenors, , drop = FALSE]
      fig <- plotly::plot_ly()

      for (market in unique(chart_data$market)) {
        df <- chart_data[chart_data$market == market, , drop = FALSE]
        fig <- fig |>
          plotly::add_lines(
            data = df,
            x = ~tenor,
            y = ~price,
            name = unique(df$label),
            line = list(width = 2.2, color = unname(palette[[market]])),
            hovertemplate = "Tenor %{x}<br>%{y:.2f}<extra>%{fullData.name}</extra>"
          )
      }

      if (!identical(overlay, "none") && !overlay %in% unique(chart_data$market)) {
        overlay_df <- tibble::tibble(
          tenor = unique(chart_data$tenor),
          price = ea_market_base_price(overlay) +
            ea_market_curve_slope(overlay) * unique(chart_data$tenor) +
            sin((unique(chart_data$tenor) + ea_market_index(overlay)) / 3) * 0.15,
          label = ea_market_labels(overlay)
        )

        fig <- fig |>
          plotly::add_lines(
            data = overlay_df,
            x = ~tenor,
            y = ~price,
            name = unique(overlay_df$label),
            line = list(width = 1.5, color = "#7f8b99", dash = "dot"),
            hovertemplate = "Tenor %{x}<br>%{y:.2f}<extra>%{fullData.name}</extra>"
          )
      }

      ea_plotly_layout(fig, x_title = "Contract month", y_title = "Price")
    })

    output$curve_history <- plotly::renderPlotly({
      chart_data <- page_data()$historical_compare
      selected_snapshot <- ea_coalesce(input$snapshot_date, "Latest")
      colors <- c(Latest = "#4da3a3", `1M Ago` = "#d2a157", `3M Ago` = "#7f8b99")
      fig <- plotly::plot_ly()

      for (snapshot in unique(chart_data$snapshot)) {
        df <- chart_data[chart_data$snapshot == snapshot, , drop = FALSE]
        fig <- fig |>
          plotly::add_lines(
            data = df,
            x = ~tenor,
            y = ~price,
            name = snapshot,
            line = list(
              width = if (identical(snapshot, selected_snapshot)) 2.6 else 1.6,
              color = unname(colors[[snapshot]])
            ),
            hovertemplate = "Tenor %{x}<br>%{y:.2f}<extra>%{fullData.name}</extra>"
          )
      }

      ea_plotly_layout(fig, x_title = "Contract month", y_title = "Price")
    })

    output$curve_heatmap <- plotly::renderPlotly({
      heatmap_data <- page_data()$curve_heatmap
      heatmap_matrix <- stats::xtabs(value ~ tenor + date, data = heatmap_data)

      fig <- plotly::plot_ly(
        x = colnames(heatmap_matrix),
        y = rownames(heatmap_matrix),
        z = unclass(heatmap_matrix),
        type = "heatmap",
        colors = c("#b35c60", "#17202b", "#4da3a3"),
        hovertemplate = "Date %{x}<br>Contract month %{y}<br>%{z:.2f}<extra>curve shift</extra>"
      )

      ea_plotly_layout(fig, x_title = NULL, y_title = "Contract month", hovermode = "closest")
    })

    output$spread_structure <- plotly::renderPlotly({
      chart_data <- page_data()$spread_structure

      fig <- plotly::plot_ly(
        data = chart_data,
        x = ~spread,
        y = ~level,
        type = "bar",
        marker = list(color = "#4da3a3"),
        customdata = ~zscore,
        hovertemplate = "%{x}<br>Level %{y:.2f}<br>Z-score %{customdata:.2f}<extra></extra>"
      )

      ea_plotly_layout(fig, x_title = NULL, y_title = "Spread", hovermode = "closest")
    })
  })
}
