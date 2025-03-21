#' Launch WLS Data Browser
#'
#' Function to open the WLS Data Browser.
#'
#' @export
wlsDataBrowser <- function() {
  old_shiny.maxRequestSize <- getOption("shiny.maxRequestSize")

  options(shiny.maxRequestSize = 1100 * 1024^2)

  ###########################
  ## UI
  ###########################
  ui <- # miniUI::miniPage(
    bslib::page_fluid(
      shiny::tags$span(shiny::icon("tag"), style = "display: none;"), # necessary to display icons in datatable
      theme = bslib::bs_theme(version = 5),
      ## Change how selected rows are highlighted. (This "hack" works in conjunction with DT::selectCells which removes selections when not used.)
      shiny::tags$head(
        shiny::tags$style(
          # shiny::HTML("table.dataTable tr.active td, table.dataTable td.active {box-shadow: inset 0 0 0 9999px #e7e7e7 !important;}"),
          # shiny::HTML("td {color: black !important;}"),
          shiny::HTML("
            :root {
              --title-box-shadow:;
              --title-box-shadow-color-rgb: 29, 31, 33;
            }
            .custom-title {
              display: flex;
              align-items: center;
              font-size: 24px;
              font-weight: bold;
              color:rgb(83, 90, 97);
              text-align: center;
              margin-top: 1rem;
              margin-bottom: 1rem;
              background-color: #f0f0f0; /* Grey background */
              border-radius: 8px; /* Rounded corners */
              padding: 10px; /* Padding for better appearance */
              box-shadow: var(--title-box-shadow, 0px 0px 2px 0px RGBA(var(--title-box-shadow-color-rgb), 0.14), 0px 2px 4px 0px RGBA(var(--title-box-shadow-color-rgb), 0.16));
            }")
        )
      ),
      ## Actual UI
      # miniUI::gadgetTitleBar("WLS Data Browser"),
      title = "WLS Data Browser",
      # shiny::tags$div(
      #   class = "custom-title",
      bslib::layout_columns(
        shiny::actionButton(inputId = "done", label = "Done"),
        "WLS Data Browser",
        col_widths = c(2, 8, -2),
        class = "custom-title"
        # )
      ),
      bslib::card(
        ## If data path not provided through options, allow user to select file
        if (is.null(getOption("wlsDataBrowser.data_path"))) {
          shiny::fileInput(
            "wls_data_path",
            label = "Choose input file",
            accept = "dta"
          )
        },
        DT::dataTableOutput("wlsData"),
        height = "90vh"
      )
    )

  ###########################
  ## Server
  ###########################
  server <- function(input, output, session) {
    ## Reactive values
    wls_data_path <- shiny::reactiveVal(getOption("wlsDataBrowser.data_path"))
    freq_table <- shiny::reactiveVal()

    ## When input$wls_data_path changes, update reactiveVal
    shiny::observe(
      wls_data_path(input$wls_data_path$datapath)
    ) |>
      shiny::bindEvent(input$wls_data_path)

    ## When wls_data_path() is ready, read metadata only, i.e. no rows!
    wls_data_tabl <- shiny::reactive({
      if (!is.null(wls_data_path())) {
        ## Read .dta file
        wls_data <- haven::read_dta(
          file = wls_data_path(),
          n_max = 0
        )

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

        # ## Add column with buttons to show values tables
        out$values <- as.character(bslib::tooltip(
          trigger = shiny::icon("table-list"),
          # "Trigger",
          "Click for table of values..."
        ))

        ## Return created table
        out
      }
    })

    ## datatable to show
    output$wlsData <- DT::renderDataTable({
      if (!is.null(wls_data_tabl())) {
        DT::datatable(
          wls_data_tabl()[, c("var_name", "visit", "labels", "values")],
          colnames = c(
            "Variable name (as in data file)" = "var_name",
            "Round" = "visit",
            "Variable Label (from data file)" = "labels",
            " " = "values"
          ),
          rownames = F,
          filter = "top",
          class = "row-border compact hover",
          options = list(
            pageLength = 20,
            lengthMenu = c(10, 20, 50, 100, 200),
            columnDefs = list(
              list(
                targets = 3,
                searchable = F,
                orderable = F,
                width = "20px"
              )
            ),
            initComplete = DT::JS("function(settings, json) {
              var table = this.api();
              $('#wlsData thead tr:eq(1)').find(\"input.form-control\").last().remove()
            }")
          ),
          escape = FALSE,
          selection = list(
            mode = "single",
            target = "cell",
            # Matrix with (row,col) that can be selected.
            # Only allow the last column to be selected
            # (Note: 0-indexed, i.e. fourth column = 3)
            selectable = cbind(1:nrow(wls_data_tabl()), 3)
          )
        )
      }
    })

    ## Create proxy data table
    wlsDataProxy <- DT::dataTableProxy("wlsData")

    ## When a row is selected and input$value_tables is checked, create frequency table
    ## for variable in row selected.
    shiny::observe({
      if (nrow(input$wlsData_cells_selected) == 1) {
        shinycssloaders::showPageSpinner()
        cur_col <- wls_data_tabl()$var_name[input$wlsData_cells_selected[1, 1]]

        freq_table(table_values(cur_col, file = wls_data_path()))
        shinycssloaders::hidePageSpinner()
      }
    }) |>
      shiny::bindEvent(input$wlsData_cells_selected)

    # shiny::observe({
    #   print(input$wlsData_cells_selected)
    #   print(nrow(input$wlsData_cells_selected) == 0)
    # }) |>
    #   shiny::bindEvent(input$wlsData_cells_selected)

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
      if (nrow(input$wlsData_cells_selected) > 0) {
        shiny::showModal(
          shiny::modalDialog(
            shiny::tagList(
              shiny::uiOutput("for_modal")
            ),
            title = shiny::HTML(with(wls_data_tabl()[input$wlsData_cells_selected[1, 1], ], paste0(var_name, ": ", labels))),
            size = "xl",
            footer = shiny::actionButton("return", "Return")
          )
        )
      } else {
        DT::selectCells(proxy = wlsDataProxy, selected = NULL)
      }
    }) |>
      shiny::bindEvent(input$wlsData_cells_selected)

    ## When return button is clicked in popup modal, remove modal and unselect row.
    shiny::observe({
      DT::selectCells(proxy = wlsDataProxy, selected = NULL)
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
