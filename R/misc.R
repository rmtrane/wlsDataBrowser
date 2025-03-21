table_values <- function(
    cur_col = "selsibtype",
    file = getOption("wlsDataPath")) {
  x <- haven::read_dta(file, col_select = dplyr::all_of(cur_col))[[1]]

  value_labels <- attr(x, "labels")
  x <- haven::zap_labels(x)

  tbl <- table(x)

  for_out <- dplyr::tibble(
    values = if (is.character(x)) names(tbl) else as.numeric(names(tbl)),
    n = as.numeric(tbl)
  )

  if (!is.null(value_labels)) {
    for_out <- merge(
      for_out,
      dplyr::tibble(
        values = unname(value_labels),
        label = names(value_labels)
      ),
      by = "values",
      all = T
    )
  }

  for_out <- within(for_out, {
    # within(for_out, {
    n[is.na(n)] <- 0
    valid <- values >= 0
    percent <- n / sum(n)
    percent_valid <- ifelse(values < 0, NA, n / sum(n[values >= 0]))
  })

  reactable::reactable(
    for_out[, intersect(c("values", "label", "valid", "n", "percent", "percent_valid"), colnames(for_out))],
    # outlined = T,
    # highlight = T,
    columns = list(
      values = reactable::colDef(
        name = "Value",
        maxWidth = 12 * 8
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
        width = 12 * 6
      ),
      n = reactable::colDef(
        name = "Frequency",
        # align = "left",
        width = 12 * nchar("Frequency"),
        format = reactable::colFormat(separators = T)
      ),
      percent = reactable::colDef(
        name = "Percent of all",
        style = \(value) bar_style(width = value),
        align = "left",
        vAlign = "top",
        format = reactable::colFormat(percent = T, digits = 1),
        width = 12 * nchar("percent of all")
      ),
      percent_valid = reactable::colDef(
        name = "Percent of valid",
        style = \(value) bar_style(width = value),
        align = "left",
        vAlign = "top",
        format = reactable::colFormat(percent = T, digits = 1),
        width = 12 * nchar("percent of valid")
      )
    )[colnames(for_out)],
    showPageSizeOptions = T,
    defaultPageSize = 25,
    pageSizeOptions = c(10, 25, 50)
  )
}


bar_style <- function(
    width = 1,
    fill = "#D9D9D9",
    height = "1.5em",
    align = c("left", "right"),
    backgroundPosition = "0.25em 0.375em",
    color = "black") {
  align <- match.arg(align)
  # vAlign <- match.arg(vAlign)
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
    # backgroundPosition = vAlign,
    backgroundPosition = backgroundPosition,
    color = color
  )
}


shinyInput <- function(FUN, len, id, ...) {
  inputs <- character(len)
  for (i in seq_len(len)) {
    inputs[i] <- as.character(FUN(paste0(id, i), ...))
  }
  inputs
}
