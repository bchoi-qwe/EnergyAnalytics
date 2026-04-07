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
  unname(purrr::map_chr(markets, ~ ea_coalesce(labels[.x], .x)))
}

ea_badge <- function(text, tone = c("neutral", "accent", "positive", "warning")) {
  tone <- match.arg(tone)
  htmltools::tags$span(class = paste("ea-badge", paste0("ea-badge--", tone)), text)
}

ea_toolbar_select <- function(input_id, label, choices, selected = NULL) {
  if (length(selected) == 0L || identical(selected, character(0)) || identical(selected, "")) {
    selected <- NULL
  }

  shiny::selectInput(
    inputId = input_id,
    label = label,
    choices = choices,
    selected = selected,
    selectize = FALSE,
    width = "100%"
  )
}

ea_history_year_choices <- function(max_years = 30L) {
  stats::setNames(as.character(seq_len(max_years)), as.character(seq_len(max_years)))
}

ea_toolbar_multiselect <- function(input_id, label, choices, selected = NULL, options = list()) {
  if (length(selected) == 0L || identical(selected, character(0)) || identical(selected, "")) {
    selected <- NULL
  }

  default_options <- list(
    plugins = list("remove_button"),
    hideSelected = FALSE,
    closeAfterSelect = FALSE
  )

  shiny::selectizeInput(
    inputId = input_id,
    label = label,
    choices = choices,
    selected = selected,
    multiple = TRUE,
    options = utils::modifyList(default_options, options),
    width = "100%"
  )
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
      reactable::reactableOutput(outputId = output_id, height = "100%", width = "100%")
    )
  )
}

ea_module_view_panel <- function(
  plot_title,
  plot_output_id,
  table_title,
  table_output_id,
  plot_height = "320px",
  col_widths = c(8, 4),
  plot_class = NULL,
  table_class = "ea-card--table"
) {
  bslib::layout_columns(
    col_widths = col_widths,
    ea_plotly_card(
      title = plot_title,
      output_id = plot_output_id,
      height = plot_height,
      class = plot_class
    ),
    ea_table_card(
      title = table_title,
      output_id = table_output_id,
      class = table_class
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

ea_focusbar <- function(eyebrow, title, chips = NULL, class = NULL) {
  chip_nodes <- if (is.null(chips)) {
    list()
  } else {
    chips[!vapply(chips, is.null, logical(1))]
  }

  htmltools::tags$div(
    class = paste(c("ea-corr-focusbar", class), collapse = " "),
    htmltools::tags$div(
      class = "ea-corr-focusbar__intro",
      htmltools::tags$div(class = "ea-corr-focusbar__eyebrow", eyebrow),
      htmltools::tags$div(class = "ea-corr-focusbar__title", title)
    ),
    htmltools::tags$div(
      class = "ea-corr-focusbar__chips",
      chip_nodes
    )
  )
}

ea_nav_registry <- function(available_only = FALSE) {
  registry <- tibble::tribble(
    ~value, ~label, ~description, ~mode, ~desk_question, ~concepts,
    "forward_curves", "Forward Curves", "Historical curve behaviour, calendar spreads, and rates overlay for one market at a time.", "Single Market",
    "Read how the forward curve has behaved, where prompt scarcity sits today, and how the selected commodity has moved against the Treasury backdrop.",
    list(c("Current curve", "Curve history", "Calendar spreads", "Rates overlay")),
    "volatility", "Volatility", "Volatility through time and across the term structure for the selected market.", "Single Market",
    "Decide whether volatility is elevated or compressed and whether that picture changes as maturity extends down the curve.",
    list(c("RV history", "Vol cone", "Volatility by tenor")),
    "options", "Options", "Vol surface shape, term Greeks, smile structure, and scenario risk for the selected market.", "Single Market",
    "Read the options surface, inspect how Greeks change down the curve, and stress the front structure with simple scenario grids.",
    list(c("Surface Greeks", "ATM term Greeks", "Smile", "P&L")),
    "codynamics", "Co-Dynamics & Rates", "Cross-market relationships plus the full Treasury curve and rate sensitivities.", "Cross-Commodity",
    "Compare markets together, isolate unstable pairs, and then connect those relationships back to the Treasury curve and rate factors.",
    list(c("Correlation matrix", "Rolling pair view", "Treasury curve", "Rate betas")),
    "seasonality", "Seasonality", "Calendar effects across markets and within a focused market.", "Hybrid",
    "Judge whether this year is tracking its seasonal template and how recurring calendar structure differs across the selected markets.",
    list(c("Seasonal overlay", "Year-on-year paths", "Seasonal spreads")),
    "hedging", "Hedging", "Hedge ratio dynamics across markets and down the term structure.", "Cross-Commodity",
    "Choose a hedge that reduces risk, inspect how stable the ratio is over time, and see whether it changes across the curve.",
    list(c("Cross hedge", "Per-tenor ratios", "Rolling hedge ratio"))
  )

  availability <- ea_page_availability()
  registry <- dplyr::left_join(registry, availability, by = "value") %>%
    dplyr::mutate(enabled = dplyr::coalesce(.data$enabled, FALSE))

  if (isTRUE(available_only)) {
    registry <- registry %>% dplyr::filter(.data$enabled)
  }

  registry
}

ea_page_metadata <- function(page_value) {
  registry <- ea_nav_registry()
  idx <- match(page_value, registry$value)

  if (is.na(idx)) {
    return(NULL)
  }

  as.list(registry[idx, , drop = FALSE])
}

ea_page_intro_panel <- function(page_value) {
  meta <- ea_page_metadata(page_value)

  if (is.null(meta)) {
    return(NULL)
  }

  concept_items <- unlist(meta$concepts, use.names = FALSE)

  mode_tone <- switch(
    meta$mode[[1]],
    "Cross-Commodity" = "accent",
    "Single Market" = "positive",
    "Hybrid" = "warning",
    "neutral"
  )

  htmltools::tags$section(
    class = "ea-page-hero",
    htmltools::tags$div(
      class = "ea-page-hero__header",
      htmltools::tags$div(
        class = "ea-page-hero__titleblock",
        htmltools::tags$div(
          class = "ea-page-hero__eyebrow",
          "Desk Workflow"
        ),
        htmltools::tags$div(
          class = "ea-page-hero__titleline",
          htmltools::tags$h1(class = "ea-page-hero__title", meta$label[[1]]),
          ea_badge(meta$mode[[1]], tone = mode_tone)
        ),
        htmltools::tags$p(class = "ea-page-hero__description", meta$description[[1]])
      ),
      htmltools::tags$div(
        class = "ea-page-hero__concepts",
        lapply(concept_items, ea_badge, tone = "neutral")
      )
    ),
    htmltools::tags$div(
      class = "ea-page-hero__question",
      htmltools::tags$span(class = "ea-page-hero__question-label", "Desk Use"),
      htmltools::tags$span(class = "ea-page-hero__question-copy", meta$desk_question[[1]])
    )
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
  htmltools::tags$header(
    class = "ea-shell-header",
    htmltools::tags$div(
      class = "ea-shell-header__brand",
      htmltools::tags$div(class = "ea-shell-header__title", "Energy Analytics")
    ),
    htmltools::tags$div(
      class = "ea-shell-header__meta",
      shiny::uiOutput("topbar_page", container = htmltools::tags$div),
      shiny::uiOutput("topbar_scope", container = htmltools::tags$div),
      htmltools::tags$div(
        class = "ea-shell-header__timestamp",
        htmltools::tags$div(
          class = "ea-topbar__timestamp",
          htmltools::tags$span(class = "ea-status-dot"),
          shiny::icon("database"),
          shiny::textOutput("header_timestamp", inline = TRUE)
        )
      )
    )
  )
}

ea_plotly_layout <- function(fig, x_title = NULL, y_title = NULL, hovermode = "x unified") {
  fig %>%
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
      margin = list(l = 60, r = 20, t = 15, b = 45),
      legend = list(
        orientation = "h",
        x = 0,
        y = -0.18,
        bgcolor = "rgba(0,0,0,0)"
      ),
      xaxis = list(
        title = list(text = ea_coalesce(x_title, ""), font = list(size = 10, color = "#6f7d8c")),
        color = "#9aa6b2",
        gridcolor = "rgba(148, 163, 184, 0.08)",
        zerolinecolor = "rgba(148, 163, 184, 0.10)"
      ),
      yaxis = list(
        title = list(text = ea_coalesce(y_title, ""), font = list(size = 10, color = "#6f7d8c")),
        color = "#9aa6b2",
        gridcolor = "rgba(148, 163, 184, 0.08)",
        zerolinecolor = "rgba(148, 163, 184, 0.10)"
      )
    ) %>%
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
      fontSize = "0.72rem",
      textTransform = "uppercase",
      letterSpacing = "0.06em",
      borderColor = "#223040",
      fontWeight = 600
    ),
    cellStyle = list(
      color = "#e6edf5",
      borderColor = "#1a2432",
      fontSize = "0.8rem",
      padding = "8px 12px"
    ),
    style = list(
      border = "1px solid #223040",
      borderRadius = "12px",
      overflow = "hidden",
      fontFamily = "IBM Plex Sans"
    )
  )
}

ea_empty_reactable <- function(message = "No data available", pagination = FALSE) {
  reactable::reactable(
    tibble::tibble(status = message),
    pagination = pagination,
    compact = TRUE,
    highlight = FALSE,
    sortable = FALSE,
    theme = ea_reactable_theme(),
    columns = list(
      status = reactable::colDef(name = "", align = "left")
    )
  )
}
