\name{run_app}
\alias{run_app}
\title{Run the Energy Analytics dashboard}
\usage{
run_app(
  onStart = NULL,
  options = list(),
  enable_bookmarking = NULL,
  uiPattern = "/"
)
}
\arguments{
  \item{onStart}{A function that will be called before the app is run.}

  \item{options}{A named list of options to pass to \code{shiny::shinyApp()}.}

  \item{enable_bookmarking}{Bookmarking mode to pass to
  \code{shiny::shinyApp()}.}

  \item{uiPattern}{URL pattern passed to \code{shiny::shinyApp()}.}
}
\value{
A Shiny app object wrapped with golem options.
}
\description{
Launch the golem-style Shiny app shell for the EnergyAnalytics package.
}
