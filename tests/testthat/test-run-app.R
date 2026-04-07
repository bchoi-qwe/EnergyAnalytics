if (!exists("ea_project_root", mode = "function")) {
  pkgload::load_all(quiet = TRUE)
}

test_that("run_app returns a Shiny app object", {
  app <- getExportedValue("EnergyAnalytics", "run_app")()
  expect_true(inherits(app, "shiny.appobj"))
})
