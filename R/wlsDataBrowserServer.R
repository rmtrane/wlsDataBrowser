#' Server Part of Shiny App
#'
wlsDataBrowserServer <- function(input, output, session) {
  ## Change max request size to make sure file can be uploaded.
  ## First, save default so we can revert when app closes.
  old_shiny.maxRequestSize <- getOption("shiny.maxRequestSize")
  options(shiny.maxRequestSize = 1100 * 1024^2)

  ## Reactive values
  wls_data_path <- shiny::reactiveVal(getOption("wlsDataBrowser.data_path"))
  freq_table <- shiny::reactiveVal()
  var_clicked <- shiny::reactiveVal()

  ## Set input.data_path_missing to TRUE if option for data_path not set.
  ## This will trigger the file input UI to show.
  observe({
    session$sendCustomMessage("set_input_value", message = list(var = "data_path_missing", val = is.null(wls_data_path())))
  })

  ## Binary to indicate if rstudioapi is available
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
  output$wlsData <- # DT::renderDataTable({
    reactable::renderReactable({
      if (!is.null(wls_data_tabl())) {
        # session$sendCustomMessage("showSpinner", TRUE)

        reactable_tbl <- reactable::reactable(
          ## Add extra columns to hold table and copy/paste icons.
          ## These will function as triggers for displaying frequency
          ## and copy variable to file
          cbind(
            wls_data_tabl()[, c("var_name", "visit", "labels")],
            "freq_table" = rep(
              ## Icon to show
              as.character(shiny::icon("th-list", lib = "glyphicon")),
              nrow(wls_data_tabl())
            ),
            "copy_to_file" = rep(
              ## Icon to show
              as.character(shiny::icon("copy", lib = "glyphicon")),
              nrow(wls_data_tabl())
            )
          ),
          columns = list(
            var_name = reactable::colDef(
              name = "Variable Name (as in data file)",
              maxWidth = 280
            ),
            visit = reactable::colDef(
              name = "Round",
              width = 90
            ),
            labels = reactable::colDef(
              name = "Variable Label (from data file)"
            ),
            freq_table = reactable::colDef(
              name = "",
              html = T,
              width = 35,
              align = "center",
              vAlign = "center",
              filterable = F,
              style = "cursor: pointer;",
              class = "freq-tables"
            ),
            copy_to_file = reactable::colDef(
              name = "",
              html = TRUE,
              width = 35,
              filterable = F,
              show = rstudioapi_available,
              align = "center",
              vAlign = "center",
              style = "cursor: pointer;",
              class = "copy-var"
            )
          ),
          onClick = reactable::JS("function(rowInfo, colInfo) {
            if (colInfo.id == 'freq_table' || colInfo.id == 'copy_to_file') {
              Shiny.setInputValue(colInfo.id, rowInfo.id);
            };
          }"),
          searchable = TRUE,
          filterable = TRUE,
          highlight = TRUE,
          showPageSizeOptions = TRUE
        )

        ## Add JS to register onStateChange. Equivalent to htmlwidgets::onRender(),
        ## but aiming at reducing dependencies
        reactable_tbl$jsHooks[["render"]] <- c(
          reactable_tbl$jsHooks[["render"]],
          list(list(code = reactable::JS("() =>{
              Reactable.onStateChange('wlsData', function(state) {
                // Set the tooltips
                setTooltips();
                setInstructiveTooltips();

                // Initiate tooltips
                $('#wlsData').find('.rt-td-inner[data-bs-original-title]').tooltip({container: 'body'});
              });
            }"), data = NULL))
        )

        reactable_tbl
      }
    })

  ## Show frequency table
  shiny::observe({
    cur_var <- wls_data_tabl()$var_name[
      as.numeric(input$freq_table) + 1
    ]

    cur_label <- wls_data_tabl()$labels[
      as.numeric(input$freq_table) + 1
    ]

    freq_tab <- table_values(cur_var, file = wls_data_path())

    shiny::showModal(
      shiny::modalDialog(
        reactable::renderReactable(freq_tab),
        title = shiny::HTML(paste0(cur_label, " (", cur_var, ")")),
        footer = shiny::actionButton("close_freq_table", "Return"),
        easyClose = F
      )
    )
  }) |>
    shiny::bindEvent(input$freq_table)

  ## Show UI to copy to active file.
  shiny::observe({
    shiny::showModal(
      shiny::modalDialog(
        shiny::p("Write new variable name to use for variable in the text input box below. If left blank, no new name will be prepended."),
        shiny::textInput("new_col_name", "New variable name", value = wls_data_tabl()$var_name[
          as.numeric(input$copy_to_file) + 1
        ]),
        footer = bslib::layout_columns(
          shiny::actionButton("close_copy", "Cancel"),
          shiny::actionButton("insert", "Insert Text"),
          col_widths = c(4, -4, 4)
        )
      )
    )
  }) |>
    shiny::bindEvent(input$copy_to_file)

  ## Actually copy to active file
  shiny::observe({
    if (rstudioapi_available) {
      ad_context <- rstudioapi::getActiveDocumentContext()

      cur_var <- wls_data_tabl()$var_name[
        as.numeric(input$copy_to_file) + 1
      ]

      if (input$new_col_name == "") {
        text_to_insert <- paste0(
          "\"", cur_var, "\" = \"", cur_var, "\""
        )
      } else {
        text_to_insert <- paste0(
          "\"", input$new_col_name, "\" = \"", cur_var, "\""
        )
      }

      if (is.null(ad_context)) {
        shiny::showNotification(
          ui = shiny::p("Something went wrong. Could not insert text. Make sure cursor is placed in a document."),
          type = "error"
        )
      } else {
        rstudioapi::insertText(
          location = ad_context$selection[[1]]$range$start,
          text = text_to_insert
        )
      }

      session$sendCustomMessage("set_input_value", message = list(var = "copy_to_file", val = NULL))

      shiny::removeModal()
    }
  }) |>
    shiny::bindEvent(input$insert)

  ## When close button is clicked, remove modal and
  ## reset input values.
  shiny::observe({
    session$sendCustomMessage("set_input_value", message = list(var = "freq_table", val = NULL))
    shiny::removeModal()
  }) |>
    shiny::bindEvent(input$close_freq_table)

  shiny::observe({
    session$sendCustomMessage("set_input_value", message = list(var = "copy_to_file", val = NULL))
    shiny::removeModal()
  }) |>
    shiny::bindEvent(input$close_copy)

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
