#' Server Part of Shiny App
#'
#' @keywords internal
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
  shiny::observe({
    session$sendCustomMessage("set_input_value", message = list(var = "data_path_missing", val = is.null(wls_data_path())))
  })

  ## Binary to indicate if rstudioapi is available
  rstudioapi_available <- requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()

  ## When input$wls_data_path changes, update reactiveVal
  shiny::observe(
    wls_data_path(input$wls_data_path$datapath)
  ) |>
    shiny::bindEvent(input$wls_data_path)


  ########################################
  ########################################
  ##
  ## Get variables
  ##
  wls_data_tabl <- shiny::reactive({
    ## When wls_data_path() is ready...
    if (!is.null(wls_data_path())) {
      ## ... read metadata only, i.e. no rows!
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
        pattern = paste0(levels(out$visit), " - ", collapse = "|"),
        replacement = "",
        x = out$labels
      )

      ## Return created table
      out
    }
  })

  ########################################
  ########################################
  ##
  ## Create main table
  ##
  output$wlsData <- reactable::renderReactable({
    if (!is.null(wls_data_tabl())) {
      reactable_tbl <- wls_variables_table(wls_data_tabl(), rstudioapi_available)

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

  ########################################
  ########################################
  ##
  ## Frequency table
  ##
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
        title = shiny::markdown(paste0("**", cur_label, "** (", cur_var, ")")),
        footer = shiny::actionButton("close_freq_table", "Return"),
        easyClose = F,
        size = "xl"
      )
    )
  }) |>
    shiny::bindEvent(input$freq_table)

  ########################################
  ########################################
  ##
  ## Copy to active file
  ##
  ## UI
  shiny::observe({
    shiny::showModal(
      shiny::modalDialog(
        shiny::p(
          "Write new variable name to use for variable in the text input box below. If left blank, no new name will be prepended."
        ),
        shiny::textInput(
          "new_col_name",
          "New variable name",
          value = wls_data_tabl()$var_name[as.numeric(input$copy_to_file) + 1]
        ),
        footer = bslib::layout_columns(
          shiny::actionButton("close_copy", "Cancel"),
          shiny::actionButton("insert", "Insert Text"),
          col_widths = c(6, 6) # c(4, -4, 4)
        ),
        size = "m"
      )
    )
  }) |>
    shiny::bindEvent(input$copy_to_file)

  ## Action
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

  ########################################
  ########################################
  ##
  ## Done/close
  ##
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
