#' Launch WLS Data Browser
#'
#' Function to open the WLS Data Browser.
#'
#' @export
wlsDataBrowserApp <- function() {
  shiny::runApp(
    app = shiny::shinyApp(
      wlsDataBrowserUI,
      wlsDataBrowserServer,
      onStart = function() {
        shiny::addResourcePath(
          "www",
          directoryPath = ifelse(dir.exists("inst/www"), "inst/www", system.file("www", package = "wlsDataBrowser"))
        )
      }
    ),
    launch.browser = shiny::paneViewer()
  )
}
