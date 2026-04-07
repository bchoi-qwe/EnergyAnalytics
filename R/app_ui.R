app_ui <- function(request) {
  supported_pages <- ea_nav_registry(available_only = TRUE)$value
  if (length(supported_pages) == 0L) {
    supported_pages <- "forward_curves"
  }

  nav_panels <- Filter(Negate(is.null), list(
    if ("forward_curves" %in% supported_pages) {
      bslib::nav_panel("Forward Curves", value = "forward_curves", mod_forward_curves_ui("forward_curves"))
    },
    if ("volatility" %in% supported_pages) {
      bslib::nav_panel("Volatility", value = "volatility", mod_volatility_ui("volatility"))
    },
    if ("options" %in% supported_pages) {
      bslib::nav_panel("Options", value = "options", mod_options_greeks_ui("options"))
    },
    if ("codynamics" %in% supported_pages) {
      bslib::nav_panel("Co-Dynamics & Rates", value = "codynamics", mod_codynamics_ui("codynamics"))
    },
    if ("seasonality" %in% supported_pages) {
      bslib::nav_panel("Seasonality", value = "seasonality", mod_seasonality_ui("seasonality"))
    },
    if ("hedging" %in% supported_pages) {
      bslib::nav_panel("Hedging", value = "hedging", mod_hedging_ui("hedging"))
    }
  ))

  htmltools::tagList(
    add_app_resources(),
    bslib::page_fillable(
      theme = ea_theme(),
      title = "Energy Analytics",
      padding = 0,
      gap = 0,
      fillable_mobile = TRUE,
      htmltools::tags$main(
        class = "ea-simple-shell",
        ea_shell_header(),
        mod_global_filters_ui("global_filters"),
        htmltools::tags$section(
          class = "ea-workspace__canvas ea-simple-navset",
          do.call(
            bslib::navset_pill,
            c(
              list(
                id = "main_nav",
                selected = supported_pages[[1]]
              ),
              nav_panels
            )
          )
        )
      )
    )
  )
}
