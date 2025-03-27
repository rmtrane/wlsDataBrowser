wlsDataBrowserUI <- bslib::page_fluid(
  ## Add JS functions to html HEAD
  shiny::tags$head(
    shiny::tags$script(
      src = "www/scripts.js",
    )
  ),
  ## Add theme
  theme = my_bs_theme(),
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
