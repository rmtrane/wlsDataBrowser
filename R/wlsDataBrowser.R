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
      theme = bslib::bs_theme(
        version = 5,
        "table-hover-bg" = "rgb(223, 223, 223)"
      ) |>
        bslib::bs_add_rules("
          /* CSS */

          /* Background color when hovering last column of wlsData */
          #wlsData tbody td:nth-child(n+4):hover {
            --bs-table-hover-bg: rgb(141, 204, 252) !important;
          }

          /* Style title */
          .custom-title {
            display: flex;
            align-items: center;
            font-size: 24px;
            font-weight: bold;
            color:rgb(83, 90, 97);
            text-align: center;
            margin-top: 0.5rem;
            margin-bottom: 0.5rem;
            background-color: #f0f0f0; /* Grey background */
            border-radius: 8px; /* Rounded corners */
            padding: 18px; /* Padding for better appearance */
          }

          /* Adjust width of modal dialog */
          .modal-dialog {
            max-width: 80% !important; /* Adjust the percentage as needed */
          }

          /* No wrap */
          .nowrap {
            white-space: nowrap;
          }

        "),
      ## Actual UI
      title = "WLS Data Browser",
      ##
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
        ## If data path not provided through options, allow user to select file
        if (is.null(getOption("wlsDataBrowser.data_path"))) {
          shiny::fileInput(
            "wls_data_path",
            label = "Choose input file",
            accept = "dta"
          )
        },
        DT::dataTableOutput("wlsData"),
        height = "85vh"
      )
    )

  ###########################
  ## Server
  ###########################
  server <- function(input, output, session) {
    ## Reactive values
    wls_data_path <- shiny::reactiveVal(getOption("wlsDataBrowser.data_path"))
    freq_table <- shiny::reactiveVal()
    var_clicked <- shiny::reactiveVal()


    rstudioapi_available <- requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()

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

        ## Return created table
        out
      }
    })

    ## datatable to show
    output$wlsData <- DT::renderDataTable({
      if (!is.null(wls_data_tabl())) {
        DT::datatable(
          ## Add extra columns to hold table and copy/paste icons.
          ## These will function as triggers for displaying frequency
          ## and copy variable to file
          cbind(
            wls_data_tabl()[, c("var_name", "visit", "labels")],
            "extra" = rep(
              ## Icon to show
              as.character(shiny::icon("table-list")),
              nrow(wls_data_tabl())
            ),
            "extra2" = rep(
              ## Icon to show
              as.character(shiny::icon("copy")),
              nrow(wls_data_tabl())
            )
          ),
          colnames = c(
            "Variable name (as in data file)" = "var_name",
            "Round" = "visit",
            "Variable Label (from data file)" = "labels",
            " " = "extra",
            " " = "extra2"
          ),
          rownames = F,
          filter = "top",
          class = "row-border compact hover",
          options = list(
            pageLength = 10,
            lengthMenu = c(10, 20, 50, 100, 200),
            columnDefs = list(
              list(targets = 4, visible = rstudioapi_available),
              list(targets = 0, width = "250px"),
              list(targets = 1, width = "50px"),
              list(
                targets = c(3, 4)[c(T, rstudioapi_available)],
                searchable = F,
                orderable = F,
                width = "20px",
                render = DT::JS(
                  "function(data, type, row, meta) {",
                  "return '<div style=\"height: 100%; width: 100%; text-align: center; vertical-align: middle;\">' + data + '</div>';",
                  "}"
                )
              )
            ),
            initComplete = DT::JS(
              "function(settings, json) {
                var table = this.api();
                $('#wlsData thead tr:eq(1)').find(\"input.form-control:disabled\").remove();
              }"
            ),
            drawCallback = DT::JS(
              "function(settings) {
                $('#wlsData tbody td:nth-child(5)').each(function() {
                  $(this).attr('title', 'Click to copy variable to active file');
                  $(this).css({
                    'text-align': 'center',
                    'vertical-align': 'middle'
                  });
                });

                $('#wlsData tbody td:nth-child(4)').each(function() {
                  $(this).attr('title', 'Click to view frequency table for observed values');
                  $(this).css({
                    'text-align': 'center',
                    'vertical-align': 'middle'
                  });
                });

                $('[title]').tooltip({
                  container: 'body',
                  delay: { show: 750, hide: 0 }
                });
              }"
            )
          ),
          escape = FALSE,
          selection = list(
            mode = "single",
            target = "cell",
            # Matrix with (row,col) that can be selected.
            # Only allow the last column to be selected
            # (Note: 0-indexed, i.e. fourth column = 3)
            selectable = rbind(cbind(1:nrow(wls_data_tabl()), 3), cbind(1:nrow(wls_data_tabl()), 4))
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
        col_clicked <- input$wlsData_cells_selected[1, 2]

        if (col_clicked == 3) {
          shinycssloaders::showPageSpinner()
          cur_col <- wls_data_tabl()$var_name[input$wlsData_cells_selected[1, 1]]

          freq_table(table_values(cur_col, file = wls_data_path()))
          shinycssloaders::hidePageSpinner()
        }
      }
    }) |>
      shiny::bindEvent(input$wlsData_cells_selected)

    ## Frequencey table for output
    output$freq_table <- reactable::renderReactable({
      if (inherits(freq_table(), "reactable")) {
        freq_table()
      }
    })

    ## Create UI for the popup modal for frequency table
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
        col_clicked <- input$wlsData_cells_selected[1, 2]
        var_clicked(wls_data_tabl()$var_name[input$wlsData_cells_selected[1, 1]])

        if (col_clicked == 3) {
          shiny::showModal(
            shiny::modalDialog(
              shiny::tagList(
                shiny::uiOutput("for_modal")
              ),
              title = shiny::HTML(with(wls_data_tabl()[input$wlsData_cells_selected[1, 1], ], paste0(var_name, ": ", labels))),
              footer = shiny::actionButton("close", "Return"),
              easyClose = T
            )
          )
        } else {
          shiny::showModal(
            shiny::modalDialog(
              shiny::p("Write new variable name to use for variable in the text input box below. If left blank, no new name will be prepended."),
              shiny::textInput("new_col_name", "New variable name"),
              footer = bslib::layout_columns(
                shiny::actionButton("close_copy", "Cancel"),
                shiny::actionButton("insert", "Insert Text"),
                col_widths = c(4, -4, 4)
              )
            )
          )
        }
      } else {
        DT::selectCells(proxy = wlsDataProxy, selected = NULL)
      }
    }) |>
      shiny::bindEvent(input$wlsData_cells_selected)

    ## When returngit  button is clicked in popup modal, remove modal and unselect row.
    shiny::observe({
      DT::selectCells(proxy = wlsDataProxy, selected = NULL)
      shiny::removeModal()
    }) |>
      shiny::bindEvent(
        input$close,
        input$close_copy,
        input$insert
      )

    shiny::observe({
      if (rstudioapi_available) {
        ad_context <- rstudioapi::getActiveDocumentContext()

        if (input$new_col_name == "") {
          text_to_insert <- var_clicked()
        } else {
          text_to_insert <- paste0(
            "\"", input$new_col_name, "\" = \"", var_clicked(), "\""
          )
        }

        rstudioapi::insertText(
          location = ad_context$selection[[1]]$range$start,
          text = text_to_insert
        )
      }
    }) |>
      shiny::bindEvent(input$insert)

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

  shiny::runGadget(ui, server)
}
