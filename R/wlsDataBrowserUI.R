#' UI Part of Shiny App
#'
wlsDataBrowserUI <- bslib::page_fluid(
  ## Add a few things to HTML
  shiny::tags$head(
    ## JS functions
    shiny::tags$script(
      src = "www/scripts.js",
    ),
    ##  CSS stylesheet
    shiny::tags$link(
      rel = "stylesheet", type = "text/css", href = "www/stylesheet.css"
    )
  ),
  ## Fix bootstrap theme to version 5
  theme = bslib::bs_theme(version = 5),
  ## Add spinner to be shown/hidden
  shiny::tags$div(id = "spinner", class = "loader"),
  ## Add title
  title = "WLS Data Browser",
  ## Actual UI
  bslib::layout_columns(
    "WLS Data Browser",
    shiny::actionButton(
      inputId = "done",
      label = "Done"
    ),
    col_widths = c(-2, 8, 2),
    class = "custom-title",
    height = "9vh"
    # )
  ),
  bslib::card(
    ## Add id to target when bluring background when spinner is displayed
    id = "content",
    ## If data path not provided through options, allow user to select file
    shiny::conditionalPanel(
      "input.data_path_missing",
      shiny::fileInput(
        "wls_data_path",
        label = "Choose input file",
        accept = "dta"
      )
    ),
    # DT::dataTableOutput("wlsData"),
    reactable::reactableOutput("wlsData"),
    height = "85vh"
  )
)
