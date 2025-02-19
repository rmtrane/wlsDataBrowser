wlsDataBrowser <- function() {
  old_shiny.maxRequestSize <- getOption("shiny.maxRequestSize")

  options(shiny.maxRequestSize = 1100 * 1024^2)

  ###########################
  ## UI
  ###########################
  ui <- miniUI::miniPage(
    theme = bslib::bs_theme(version = 4),
    ## Change how selected rows are highlighted. (This "hack" works in conjunction with DT::selectRows which removes selections when not used.)
    shiny::tags$style(
      shiny::HTML("table.dataTable tr.active td, table.dataTable td.active {box-shadow: inset 0 0 0 9999px #e7e7e7 !important;}"),
      shiny::HTML("td {color: black !important;}")
    ),
    miniUI::gadgetTitleBar("WLS Data Browser"),
    miniUI::miniContentPanel(
      ## If data path not provided through options, allow user to select file
      if (is.null(getOption("wlsDataBrowser.data_path"))) {
        shiny::fileInput(
          "wls_data_path",
          label = "Choose input file",
          accept = "dta"
        )
      },
      shiny::checkboxInput(
        "value_tables",
        "Show tables of values in data when rows are clicked (warning: slow!)",
        width = "100%"
      ),
      DT::dataTableOutput("wlsData")
    )
  )

  ###########################
  ## Server
  ###########################
  server <- function(input, output, session) {
    ## Reactive values
    wls_data_path <- shiny::reactiveVal(getOption("wlsDataBrowser.data_path"))
    # wls_data_tabl <- shiny::reactiveVal(getOption("wlsDataBrowser.data_path"))
    freq_table <- shiny::reactiveVal()

    ## When input$wls_data_path changes, update reactiveVal
    shiny::observe(
      wls_data_path(input$wls_data_path$datapath)
    ) |>
      shiny::bindEvent(input$wls_data_path)

    ## When wls_data_path() is ready, read metadata, i.e. no rows!
    wls_data_tabl <- shiny::reactive({
      if (!is.null(wls_data_path())) {
        wls_data <- haven::read_dta(file = wls_data_path(), n_max = 0)

        ## Create table with variable names and labels
        out <- data.frame(
          var_name = colnames(wls_data),
          labels = unname(unlist(lapply(wls_data, \(x) attr(x, "label")))),
          stringsAsFactors = F
        )

        ## Create visit (Round) column
        out$visit <- factor(
          unlist(lapply(strsplit(out$labels, " - "), `[[`, 1)),
          levels = c("SD", "AD", paste0("R0", 1:8))
        )

        ## Remove visit (Round) from labels
        out$labels <- gsub(
          pattern = paste(levels(out$visit), collapse = " - |"),
          replacement = "",
          x = out$labels
        )

        ## Update reactiveVal with created table
        out
      }
    })

    ## datatable to show
    output$wlsData <- DT::renderDataTable({
      if (!is.null(wls_data_tabl())) {
        DT::datatable(
          wls_data_tabl()[, c("var_name", "visit", "labels")],
          colnames = c(
            "Variable name (as in data file)" = "var_name",
            "Round" = "visit",
            "Variable Label (from data file)" = "labels"
          ),
          rownames = F,
          filter = "top",
          class = "row-border compact hover",
          options = list(
            pageLength = 20,
            lengthMenu = c(10, 20, 50, 100, 200)
          ),
          selection = "single"
        )
      }
    })

    ## Create proxy data table
    wlsDataProxy <- DT::dataTableProxy("wlsData")

    ## When a row is selected and input$value_tables is checked, create frequency table
    ## for variable in row selected.
    shiny::observe({
      if (!is.null(input$wlsData_rows_selected)) {
        if (input$value_tables) {
          shinycssloaders::showPageSpinner()
          cur_col <- wls_data_tabl()$var_name[input$wlsData_rows_selected]

          freq_table(table_values(cur_col, file = wls_data_path()))
          shinycssloaders::hidePageSpinner()
        }
      }
    }) |>
      shiny::bindEvent(input$wlsData_rows_selected)

    ## Frequencey table for output
    output$freq_table <- reactable::renderReactable({
      if (inherits(freq_table(), "reactable")) {
        freq_table()
      }
    })

    ## Create UI for the popup modal
    output$for_modal <- shiny::renderUI({
      if (inherits(freq_table(), "reactable")) {
        reactable::reactableOutput("freq_table")
      } else {
        shiny::h2(freq_table())
      }
    })

    ## Show modal, or remove selection
    shiny::observe({
      if (input$value_tables) {
        shiny::showModal(
          shiny::modalDialog(
            shiny::tagList(
              shiny::uiOutput("for_modal")
            ),
            title = shiny::HTML(with(wls_data_tabl()[input$wlsData_rows_selected, ], paste0(var_name, ": ", labels))),
            size = "xl",
            footer = shiny::actionButton("return", "Return")
          )
        )
      } else {
        DT::selectRows(proxy = wlsDataProxy, selected = NULL)
      }
    }) |>
      shiny::bindEvent(input$wlsData_rows_selected)

    ## When return button is clicked in popup modal, remove modal and unselect row.
    shiny::observe({
      DT::selectRows(proxy = wlsDataProxy, selected = NULL)
      shiny::removeModal()
    }) |>
      shiny::bindEvent(input$return)


    ## When user clicks "Done", reset maxRequestSize
    shiny::observe({
      shiny::stopApp()
      options(shiny.maxRequestSize = old_shiny.maxRequestSize)
    }) |>
      shiny::bindEvent(input$done)

    ## If window is closed, reset maxRequestSize
    session$onSessionEnded(function() {
      shiny::stopApp()
      options(shiny.maxRequestSize = old_shiny.maxRequestSize)
    })
  }

  viewer <- shiny::browserViewer()
  shiny::runGadget(ui, server, viewer = viewer)
}
