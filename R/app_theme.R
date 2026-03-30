# Theme tuned for a dark, institutional analytics surface.
ea_theme <- function() {
  bslib::bs_theme(
    version = 5,
    bg = "#0b0f14",
    fg = "#e6edf5",
    primary = "#4da3a3",
    secondary = "#7f8b99",
    success = "#65b58b",
    info = "#5a85c8",
    warning = "#d2a157",
    danger = "#c96b6b",
    base_font = bslib::font_google("IBM Plex Sans"),
    heading_font = bslib::font_google("IBM Plex Sans"),
    code_font = bslib::font_google("IBM Plex Mono"),
    "body-bg" = "#0b0f14",
    "body-color" = "#e6edf5",
    "card-bg" = "#111822",
    "card-border-color" = "#223040",
    "dropdown-bg" = "#111822",
    "dropdown-link-color" = "#e6edf5",
    "dropdown-link-hover-bg" = "#172231",
    "input-bg" = "#101722",
    "input-color" = "#e6edf5",
    "input-border-color" = "#223040",
    "nav-link-color" = "#9aa6b2",
    "nav-link-hover-color" = "#e6edf5",
    "nav-tabs-border-color" = "#223040",
    "border-radius" = ".85rem",
    "border-radius-sm" = ".65rem"
  )
}
