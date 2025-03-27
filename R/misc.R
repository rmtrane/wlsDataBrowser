#' Frequency Table using reactable
#'
#' @param cur_col string; name of column to create frequency table from
#' @param file string; path to file
#'
table_values <- function(
    cur_col = "selsibtype",
    file = getOption("wlsDataBrowser.data_path")) {
  col_names <- names(haven::read_dta(file, n_max = 0))

  x <- haven::read_dta(file, col_select = which(col_names == cur_col))[[cur_col]]

  value_labels <- attr(x, "labels")
  x <- haven::zap_labels(x)

  tbl <- table(x)

  for_out <- data.frame(
    values = if (is.character(x)) names(tbl) else as.numeric(names(tbl)),
    n = as.numeric(tbl)
  )

  if (!is.null(value_labels)) {
    for_out <- merge(
      for_out,
      data.frame(
        values = unname(value_labels),
        label = names(value_labels)
      ),
      by = "values",
      all = T
    )
  }

  for_out <- within(for_out, {
    n[is.na(n)] <- 0
    valid <- values >= 0
    percent <- n / sum(n)
    percent_valid <- ifelse(values < 0, NA, n / sum(n[values >= 0]))
  })

  reactable::reactable(
    # Use intersect since not all columns are always present.
    for_out[, intersect(c("values", "label", "valid", "n", "percent", "percent_valid"), colnames(for_out))],
    columns = list(
      values = reactable::colDef(
        name = "Value",
        headerClass = "nowrap",
        minWidth = 12 * 6
      ),
      label = reactable::colDef(name = "Label"),
      valid = reactable::colDef(
        name = "Valid",
        cell = function(value) {
          if (value) {
            shiny::tagAppendAttributes(
              shiny::icon("circle-check", class = "fas"),
              style = "color: green;"
            )
          } else {
            shiny::tagAppendAttributes(
              shiny::icon("circle-xmark", class = "fas"),
              style = "color: red;"
            )
          }
        },
        align = "center",
        vAlign = "top",
        width = 12 * 5
      ),
      n = reactable::colDef(
        name = "Frequency",
        width = 11 * nchar("Frequency"),
        format = reactable::colFormat(separators = T),
        headerClass = "nowrap"
      ),
      percent = reactable::colDef(
        name = "Percent of all",
        style = \(value) bar_style(width = value),
        align = "left",
        vAlign = "top",
        format = reactable::colFormat(percent = T, digits = 1),
        headerClass = "nowrap",
        width = 9 * nchar("percent of all")
      ),
      percent_valid = reactable::colDef(
        name = "Percent of valid",
        style = \(value) bar_style(width = value),
        align = "left",
        vAlign = "top",
        format = reactable::colFormat(percent = T, digits = 1),
        headerClass = "nowrap",
        width = 9 * nchar("percent of valid")
      )
    )[colnames(for_out)], # Only keep colDefs for columns in for_out
    showPageSizeOptions = T,
    defaultPageSize = 25,
    pageSizeOptions = c(10, 25, 50),
    bordered = T
  )
}

#' Create bars for percentages
#'
#' @param width scalar; width of bar as proportion of cell
#' @param fill string; hex color to use for bar
#' @param height string; height of bar
#' @param align string; one of "left" or "right" to indicate alignment of bar
#' @param backgroundPosition string; adjust position of bar
#' @param color string; color to use for text
#'
bar_style <- function(
    width = 1,
    fill = "#D9D9D9",
    height = "1.5em",
    align = c("left", "right"),
    backgroundPosition = "0.25em 0.375em",
    color = "black") {
  align <- match.arg(align)

  if (align == "left") {
    position <- paste0(width * 100, "%")
    image <- sprintf("linear-gradient(90deg, %1$s %2$s, transparent %2$s)", fill, position)
  } else {
    position <- paste0(100 - width * 100, "%")
    image <- sprintf("linear-gradient(90deg, transparent %1$s, %2$s %1$s)", position, fill)
  }
  list(
    backgroundImage = image,
    backgroundSize = paste("100%", height),
    backgroundRepeat = "no-repeat",
    backgroundPosition = backgroundPosition,
    color = color
  )
}
