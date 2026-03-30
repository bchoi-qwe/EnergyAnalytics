project_root <- if (file.exists("DESCRIPTION")) "." else ".."
project_root <- normalizePath(project_root, winslash = "/", mustWork = TRUE)

source(file.path(project_root, "scripts", "_common.R"), local = TRUE)

project_root <- ea_enter_project(project_root)
ea_require_packages("pkgload")
ea_load_dev_package(project_root)

latest_dir <- ea_latest_dir(project_root)
release_dir <- ea_release_dir(project_root)
