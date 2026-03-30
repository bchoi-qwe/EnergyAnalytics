mod_footer_notes_ui <- function(id) {
  shiny::uiOutput(shiny::NS(id, "footer"))
}

mod_footer_notes_server <- function(id, notes, title = "Methodology / Notes") {
  shiny::moduleServer(id, function(input, output, session) {
    output$footer <- shiny::renderUI({
      note_bundle <- notes()

      ea_methodology_footer(
        title = title,
        notes = note_bundle$notes,
        assumptions = note_bundle$assumptions,
        timestamp = note_bundle$timestamp
      )
    })
  })
}
