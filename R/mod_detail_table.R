mod_detail_table_ui <- function(id) {
  shiny::uiOutput(shiny::NS(id, "table_card"))
}

mod_detail_table_server <- function(id, data, title = "Detail table", subtitle = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    output$table_card <- shiny::renderUI({
      ea_table_card(
        title = title,
        subtitle = subtitle,
        output_id = session$ns("detail_table"),
        class = "ea-card--table"
      )
    })

    output$detail_table <- reactable::renderReactable({
      table_data <- data()
      shiny::req(table_data)

      reactable::reactable(
        data = table_data,
        theme = ea_reactable_theme(),
        compact = TRUE,
        searchable = TRUE,
        highlight = TRUE,
        resizable = TRUE,
        sortable = TRUE,
        pagination = nrow(table_data) > 8L,
        defaultPageSize = min(8L, nrow(table_data))
      )
    })
  })
}
