#' Bird Layout Front Page
#' @param title The title to display in the front page
#' @param subtitle The subtitle to display in the front page
#' @param background The background to display in the front page
#' @param ... additional arguments
#' @importFrom shiny div
#' @export
frontPage <- function(title = "My Shiny App", subtitle = NULL, background = NULL, ...) {
  div(
    class = "front-page",
    frontTitle(title, subtitle),
    frontBackground(background),
    ...
  )
}

#' Bird Layout Front Title
#' @importFrom shiny div h1 h2
#' @keywords internal
frontTitle <- function(title, subtitle) {
  div(
    class = "front-page",
    div(class = "img-front-page"),
    div(class = "title-front-page",
        h1(title),
        if (!is.null(subtitle)) h2(subtitle) else NULL
    )
  )
}

#' Bird Layout Background Image
#' @importFrom shiny tags HTML
#' @keywords internal
frontBackground <- function(background) {
  if (!is.null(background)) {
    tags$head(
      tags$style(
        HTML(sprintf(
          "div.front-page>div.img-front-page { background-image: url('%s') !important; }",
          background
        ))
      ))
  } else {
    NULL
  }
}
