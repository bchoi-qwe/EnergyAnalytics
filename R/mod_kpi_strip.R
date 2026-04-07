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

      pills <- lapply(seq_len(nrow(kpi_data)), function(i) {
        row <- kpi_data[i, ]

        htmltools::tags$div(
          class = "ea-kpi-pill",
          htmltools::tags$div(class = "ea-kpi-pill__label", row$title),
          htmltools::tags$div(
            class = "ea-kpi-pill__body",
            htmltools::tags$div(class = "ea-kpi-pill__value", row$value),
            htmltools::tags$div(class = "ea-kpi-pill__delta", row$delta)
          )
        )
      })

      htmltools::tags$div(
        class = "ea-kpi-strip",
        pills
      )
    })
  })
}
