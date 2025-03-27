#' Bootstrap Theme for wlsDataBrowser
#'
my_bs_theme <- function() {
  bslib::bs_theme(
    version = 5
  ) |>
    bslib::bs_add_rules(
      rules =
        paste0(
          readLines(
            ifelse(file.exists("inst/www/stylesheet.css"), "inst/www/stylesheet.css", "www/stylesheet.css")
          ),
          collapse = "\n"
        )
    )
}
