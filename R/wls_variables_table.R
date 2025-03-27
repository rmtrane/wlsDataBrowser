#' Table of WLS Variables
#'
#' @param wls_dat `data.frame` with columns `var_name`, `visit`, and `labels`
#' @param rstudioapi_available `logical` to indicate if the R package `rstudioapi` is installed and available.
#'
#' @keywords internal
wls_variables_table <- function(
    wls_dat,
    rstudioapi_available = F) {
  reactable::reactable(
    ## Add extra columns to hold table and copy/paste icons.
    ## These will function as triggers for displaying frequency
    ## and copy variable to file
    cbind(
      wls_dat[, c("var_name", "visit", "labels")],
      "freq_table" = rep(
        ## Icon to show
        as.character(shiny::icon("th-list", lib = "glyphicon")),
        nrow(wls_dat)
      ),
      "copy_to_file" = rep(
        ## Icon to show
        as.character(shiny::icon("copy", lib = "glyphicon")),
        nrow(wls_dat)
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
    onClick = if (shiny::isRunning()) reactable::JS("(rowInfo, colInfo) => { Shiny.setInputValue(colInfo.id, rowInfo.id); }"),
    searchable = TRUE,
    filterable = TRUE,
    highlight = TRUE,
    showPageSizeOptions = TRUE
  )
}
