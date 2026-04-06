ea_accent_color <- function() {
  "#4da3a3"
}

ea_market_palette <- function() {
  c(
    CL = "#4da3a3",
    NG = "#5a85c8",
    HTT = "#74a66e",
    BRN = "#d2a157",
    HO = "#b98054",
    RB = "#d36e70",
    UST = "#7f8b99"
  )
}

ea_coalesce <- function(x, y) {
  if (is.null(x) || (length(x) == 0L) || identical(x, "") || identical(x, character(0))) {
    return(y)
  }

  x
}

ea_format_timestamp <- function(timestamp) {
  if (is.null(timestamp) || length(timestamp) == 0L || is.na(timestamp)) {
    return("Timestamp pending")
  }

  if (inherits(timestamp, "POSIXt")) {
    return(format(timestamp, "%d %b %Y %H:%M %Z"))
  }

  as.character(timestamp)
}

ea_market_labels <- function(markets, catalog = ea_market_catalog()) {
  if (length(markets) == 0L) {
    return(character(0))
  }

  labels <- stats::setNames(catalog$label, catalog$market)
  unname(labels[markets])
}

ea_badge <- function(text, tone = c("neutral", "accent", "positive", "warning")) {
  tone <- match.arg(tone)
  htmltools::tags$span(class = paste("ea-badge", paste0("ea-badge--", tone)), text)
}

ea_loading_wrapper <- function(content) {
  shinycssloaders::withSpinner(
    ui_element = content,
    type = 4,
    color = ea_accent_color(),
    color.background = "transparent",
    proxy.height = "280px"
  )
}

ea_standard_card <- function(title, ..., subtitle = NULL, class = NULL, full_screen = TRUE) {
  header <- htmltools::tagList(
    htmltools::div(class = "ea-card__title", title),
    if (!is.null(subtitle)) {
      htmltools::div(class = "ea-card__subtitle", subtitle)
    }
  )

  bslib::card(
    full_screen = full_screen,
    class = paste(c("ea-card", class), collapse = " "),
    bslib::card_header(header),
    bslib::card_body(
      fillable = TRUE,
      class = "ea-card__body",
      ...
    )
  )
}

ea_plotly_card <- function(title, output_id, subtitle = NULL, height = "320px", class = NULL) {
  ea_standard_card(
    title = title,
    subtitle = subtitle,
    class = class,
    ea_loading_wrapper(
      plotly::plotlyOutput(outputId = output_id, height = height)
    )
  )
}

ea_table_card <- function(title, output_id, subtitle = NULL, class = NULL) {
  ea_standard_card(
    title = title,
    subtitle = subtitle,
    class = class,
    ea_loading_wrapper(
      reactable::reactableOutput(outputId = output_id, height = "100%")
    )
  )
}

ea_empty_state_card <- function(title, body, hint = NULL) {
  bslib::card(
    class = "ea-card ea-card--empty",
    full_screen = FALSE,
    bslib::card_body(
      class = "ea-empty-state",
      shiny::icon("chart-line"),
      htmltools::tags$h3(title),
      htmltools::tags$p(body),
      if (!is.null(hint)) {
        htmltools::tags$p(class = "ea-empty-state__hint", hint)
      }
    )
  )
}

ea_methodology_footer <- function(title = "Methodology / Notes", notes = NULL, assumptions = NULL, timestamp = NULL) {
  note_nodes <- if (length(notes) > 0L) {
    htmltools::tags$ul(class = "ea-footer-notes__list", lapply(notes, htmltools::tags$li))
  }

  assumption_nodes <- if (length(assumptions) > 0L) {
    htmltools::tags$ul(class = "ea-footer-notes__list", lapply(assumptions, htmltools::tags$li))
  }

  ea_standard_card(
    title = title,
    class = "ea-card--footer",
    full_screen = FALSE,
    htmltools::tagList(
      if (length(notes) > 0L) {
        htmltools::tags$div(
          class = "ea-footer-notes__section",
          htmltools::tags$div(class = "ea-footer-notes__label", "Notes"),
          note_nodes
        )
      },
      if (length(assumptions) > 0L) {
        htmltools::tags$div(
          class = "ea-footer-notes__section",
          htmltools::tags$div(class = "ea-footer-notes__label", "Assumptions"),
          assumption_nodes
        )
      },
      if (!is.null(timestamp)) {
        htmltools::tags$div(
          class = "ea-footer-notes__timestamp",
          shiny::icon("clock"),
          paste("Data as of", ea_format_timestamp(timestamp))
        )
      }
    )
  )
}

ea_selected_filters_ribbon <- function(filters, data_timestamp) {
  market_labels <- ea_market_labels(filters$commodities)
  benchmark_label <- ea_market_labels(filters$comparison_commodity)

  summary_badges <- list(
    if (!identical(filters$market_complex, "Cross-Commodity")) {
      ea_badge(filters$market_complex, tone = "accent")
    },
    if (length(market_labels) == 0L) {
      ea_badge("No markets selected", tone = "warning")
    } else {
      lapply(market_labels, ea_badge, tone = "neutral")
    },
    if (length(benchmark_label) > 0L) {
      ea_badge(paste("Benchmark", benchmark_label), tone = "neutral")
    },
    ea_badge(paste("Lookback", filters$rolling_window), tone = "neutral")
  )

  htmltools::tags$div(
    class = "ea-filter-ribbon",
    htmltools::tags$div(
      class = "ea-filter-ribbon__items",
      summary_badges
    ),
    htmltools::tags$span(
      class = "ea-filter-ribbon__timestamp",
      shiny::icon("clock"),
      paste("Data as of", ea_format_timestamp(data_timestamp))
    )
  )
}

ea_nav_registry <- function() {
  tibble::tribble(
    ~value, ~code, ~label, ~description,
    "overview", "MM", "Market Monitor", "Outrights, spreads, and benchmark context",
    "fundamentals", "FD", "Fundamentals", "Inventories, balances, and news flow",
    "forward_curves", "TS", "Term Structure", "Curve shape, curve history, and spreads",
    "volatility", "VS", "Vol Surface", "ATMF term structure and smile",
    "options_greeks", "OG", "Options Greeks", "Black-76 Greeks across the vol surface",
    "codynamics", "CR", "Correlations", "Rolling correlation and intercommodity spread",
    "seasonality", "SE", "Seasonality", "Calendar structure and seasonal spreads",
    "hedging", "CH", "Cross-Hedge", "Hedge ratios, basis, and effectiveness",
    "scenarios", "SA", "Scenario Analysis", "Shock inputs and impact attribution"
  )
}

ea_app_rail <- function() {
  htmltools::tags$aside(
    class = "ea-rail",
    htmltools::tags$div(
      class = "ea-rail__brand",
      htmltools::tags$div(class = "ea-rail__brand-mark", "EA"),
      htmltools::tags$div(
        class = "ea-rail__brand-copy",
        htmltools::tags$div(class = "ea-rail__brand-title", "Energy Analytics")
      )
    ),
    htmltools::tags$div(
      class = "ea-rail__section",
      htmltools::tags$div(class = "ea-rail__section-label", "Views"),
      shiny::uiOutput("rail_nav", container = htmltools::tags$div)
    )
  )
}

ea_shell_header <- function() {
  htmltools::tags$div(
    class = "ea-topbar",
    shiny::uiOutput("topbar_page", container = htmltools::tags$div),
    shiny::uiOutput("topbar_scope", container = htmltools::tags$div),
    htmltools::tags$div(
      class = "ea-topbar__actions",
      htmltools::tags$div(
        class = "ea-topbar__timestamp",
        htmltools::tags$span(class = "ea-status-dot"),
        shiny::icon("database"),
        shiny::textOutput("header_timestamp", inline = TRUE)
      ),
      shiny::actionButton(
        inputId = "refresh_data",
        label = "Refresh",
        icon = shiny::icon("rotate"),
        class = "btn btn-sm btn-outline-secondary ea-header-btn"
      ),
      shiny::actionButton(
        inputId = "reset_filters",
        label = "Reset filters",
        icon = shiny::icon("eraser"),
        class = "btn btn-sm btn-outline-secondary ea-header-btn"
      ),
      shiny::actionButton(
        inputId = "export_view",
        label = "Export",
        icon = shiny::icon("file-export"),
        class = "btn btn-sm btn-outline-secondary ea-header-btn"
      )
    )
  )
}

ea_plotly_layout <- function(fig, x_title = NULL, y_title = NULL, hovermode = "x unified") {
  fig |>
    plotly::layout(
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      hovermode = hovermode,
      hoverlabel = list(
        bgcolor = "#121a23",
        bordercolor = "#314252",
        font = list(
          color = "#e6edf5",
          family = "IBM Plex Sans",
          size = 11
        ),
        namelength = -1
      ),
      font = list(color = "#e6edf5", family = "IBM Plex Sans", size = 11),
      margin = list(l = 50, r = 18, t = 10, b = 42),
      legend = list(
        orientation = "h",
        x = 0,
        y = -0.15,
        bgcolor = "rgba(0,0,0,0)"
      ),
      xaxis = list(
        title = ea_coalesce(x_title, ""),
        color = "#9aa6b2",
        gridcolor = "rgba(148, 163, 184, 0.08)",
        zerolinecolor = "rgba(148, 163, 184, 0.10)"
      ),
      yaxis = list(
        title = ea_coalesce(y_title, ""),
        color = "#9aa6b2",
        gridcolor = "rgba(148, 163, 184, 0.08)",
        zerolinecolor = "rgba(148, 163, 184, 0.10)"
      )
    ) |>
    plotly::config(
      displayModeBar = FALSE,
      displaylogo = FALSE,
      responsive = TRUE,
      toImageButtonOptions = list(format = "png", filename = "energy-analytics-chart")
    )
}

ea_reactable_theme <- function() {
  reactable::reactableTheme(
    backgroundColor = "transparent",
    borderColor = "#223040",
    stripedColor = "rgba(255, 255, 255, 0.02)",
    highlightColor = "rgba(77, 163, 163, 0.12)",
    headerStyle = list(
      background = "#101722",
      color = "#9aa6b2",
      fontSize = "0.76rem",
      textTransform = "uppercase",
      letterSpacing = "0.06em",
      borderColor = "#223040"
    ),
    cellStyle = list(
      color = "#e6edf5",
      borderColor = "#1a2432",
      fontSize = "0.84rem"
    ),
    style = list(
      border = "1px solid #223040",
      borderRadius = "12px",
      overflow = "hidden"
    )
  )
}
