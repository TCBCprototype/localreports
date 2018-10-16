#' Run an instance of the website
#'
#' @export
runDemo <- function() {
  appDir <- system.file("demo", package = "localreports")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `localreports`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
