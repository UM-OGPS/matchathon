run_app <- function() {
  appDir <- system.file("shiny", "matchathon", package = "matchathon")
  if (appDir == "") {
    stop("Could not find directory. Try re-installing `matchathon`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
