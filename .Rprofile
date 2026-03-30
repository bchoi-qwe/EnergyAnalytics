local({
  project_root <- normalizePath(".", winslash = "/", mustWork = TRUE)
  options(EnergyAnalytics.project_root = project_root)

  startup_file <- file.path(project_root, "scripts", "_startup.R")
  if (file.exists(startup_file)) {
    source(startup_file, local = FALSE)
    ea_use_project_repos()
  }

  renv_activate <- file.path(project_root, "renv", "activate.R")
  if (file.exists(renv_activate)) {
    source(renv_activate)
  }

  if (exists("ea_activate_project_startup", mode = "function")) {
    ea_activate_project_startup(project_root)
  }
})
