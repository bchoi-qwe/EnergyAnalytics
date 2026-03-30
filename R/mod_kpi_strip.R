mod_kpi_strip_ui <- function(id) {
  shiny::uiOutput(shiny::NS(id, "kpi_strip"))
}

mod_kpi_strip_server <- function(id, kpis) {
  shiny::moduleServer(id, function(input, output, session) {
    output$kpi_strip <- shiny::renderUI({
      kpi_data <- kpis()
      if (is.null(kpi_data) || nrow(kpi_data) == 0L) {
        return(NULL)
      }

      width <- max(floor(12 / nrow(kpi_data)), 2)

      boxes <- lapply(seq_len(nrow(kpi_data)), function(i) {
        row <- kpi_data[i, ]

        bslib::value_box(
          title = row$title,
          value = htmltools::tags$div(class = "ea-kpi-box__value", row$value),
          htmltools::tags$div(class = "ea-kpi-box__delta", row$delta),
          full_screen = FALSE,
          min_height = "120px",
          class = paste("ea-kpi-box", paste0("ea-kpi-box--", row$status))
        )
      })

      do.call(
        bslib::layout_columns,
        c(
          boxes,
          list(
            col_widths = rep(width, length(boxes)),
            fill = FALSE,
            fillable = TRUE,
            class = "ea-kpi-strip"
          )
        )
      )
    })
  })
}
