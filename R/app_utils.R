app_sys <- function(...) {
  installed_path <- system.file(..., package = "EnergyAnalytics")
  if (nzchar(installed_path)) {
    return(installed_path)
  }

  file.path(ea_project_root(), "inst", ...)
}

add_app_resources <- function() {
  resource_name <- "ea-www"
  resource_dir <- app_sys("app/www")
  existing_paths <- shiny::resourcePaths()

  if (
    dir.exists(resource_dir) &&
      (!resource_name %in% names(existing_paths) ||
        !identical(unname(existing_paths[[resource_name]]), resource_dir))
  ) {
    shiny::addResourcePath(resource_name, resource_dir)
  }

  htmltools::tagList(
    shiny::tags$head(
      shiny::tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = file.path(resource_name, "energy-analytics.css")
      )
    )
  )
}
