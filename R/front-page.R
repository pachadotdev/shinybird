#' Create a styled front page
#'
#' Create a front page with title, subtitle and background image.
#'
#' @param title The title to display in the front page
#' @param subtitle The subtitle to display in the front page
#' @param background The background to display in the front page
#' @param ... additional arguments
#'
#' @examples
#' frontPage("Much content", "Very nice")
#'
#' @importFrom shiny div
#'
#' @export
frontPage <- function(title = "My Shiny App", subtitle = NULL, background = NULL, ...) {
  div(
    class = "front-page",
    frontTitle(title, subtitle),
    frontBackground(background),
    ...
  )
}
