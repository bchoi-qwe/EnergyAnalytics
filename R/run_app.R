# UI stack recommendations:
# - shiny: runtime and reactive framework for the dashboard shell.
# - golem: production app entrypoint and resource handling.
# - bslib: Bootstrap 5 layout, theming, and cards.
# - shinyWidgets: compact, searchable filter controls.
# - plotly: interactive chart placeholders and future linked views.
# - reactable: dense analytical tables for drill-down and hedge matrices.
# - shinycssloaders: lightweight loading states for expensive outputs.

#' Run the Energy Analytics dashboard
#'
#' Launches the golem-style Shiny app shell for the EnergyAnalytics package.
#'
#' @param onStart A function that will be called before the app is run.
#' @param options A named list of options to pass to \code{shiny::shinyApp()}.
#' @param enable_bookmarking Bookmarking mode to pass to \code{shiny::shinyApp()}.
#' @param uiPattern URL pattern passed to \code{shiny::shinyApp()}.
#'
#' @return A Shiny app object wrapped with golem options.
run_app <- function(
  onStart = NULL,
  options = list(),
  enable_bookmarking = NULL,
  uiPattern = "/"
) {
  golem::with_golem_options(
    app = shiny::shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enable_bookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list()
  )
}
