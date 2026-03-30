app_ui <- function(request) {
  htmltools::tagList(
    add_app_resources(),
    bslib::page_fillable(
      theme = ea_theme(),
      title = "Energy Analytics",
      padding = 0,
      gap = 0,
      fillable_mobile = TRUE,
      htmltools::tags$div(
        class = "ea-app-shell",
        ea_app_rail(),
        htmltools::tags$main(
          class = "ea-workspace",
          ea_shell_header(),
          mod_global_filters_ui("global_filters"),
          htmltools::tags$div(
            class = "ea-workspace__canvas",
            bslib::navset_hidden(
              id = "main_nav",
              selected = "overview",
              bslib::nav_panel("Market Monitor", value = "overview", mod_overview_ui("overview")),
              bslib::nav_panel("Fundamentals", value = "fundamentals", mod_fundamentals_ui("fundamentals")),
              bslib::nav_panel("Term Structure", value = "forward_curves", mod_forward_curves_ui("forward_curves")),
              bslib::nav_panel("Vol Surface", value = "volatility", mod_volatility_ui("volatility")),
              bslib::nav_panel("Correlations", value = "codynamics", mod_codynamics_ui("codynamics")),
              bslib::nav_panel("Seasonality", value = "seasonality", mod_seasonality_ui("seasonality")),
              bslib::nav_panel("Cross-Hedge", value = "hedging", mod_hedging_ui("hedging")),
              bslib::nav_panel("Scenario Analysis", value = "scenarios", mod_scenarios_ui("scenarios"))
            )
          )
        )
      )
    )
  )
}
