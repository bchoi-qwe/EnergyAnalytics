setup_script <- if (file.exists(file.path("data-raw", "00_setup.R"))) {
  file.path("data-raw", "00_setup.R")
} else {
  "00_setup.R"
}

source(setup_script)

ea_build_latest_snapshot(
  latest_dir = file.path(project_root, "inst", "extdata", "latest"),
  release_dir = file.path(project_root, "artifacts", "releases")
)
