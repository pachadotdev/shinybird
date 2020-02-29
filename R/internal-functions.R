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
frontBackground <- function(background, background_scale) {
  if (!is.null(background)) {
    tags$head(
      tags$style(
        HTML(
          if (background_scale == TRUE) {
            sprintf(
              "div.front-page>div.img-front-page {background-image: url('%s') !important;
              background-repeat: no-repeat; background-size: cover;}",
              background
            )
          } else {
            sprintf(
              "div.front-page>div.img-front-page {background-image: url('%s') !important;}",
              background
            )
          }
        )
      ))
  } else {
    NULL
  }
}

#' Styled Title for Tab
#' @importFrom shiny h1
#' @keywords internal
tabTitle <- function(title) {
  h1(class = "tabTitle", title)
}

#' Styled Description for Tab
#' @importFrom shiny tags
#' @keywords internal
tabDesc <- function(description) {
  if (!is.null(description)) tags$h2(class = "tabDesc", description) else NULL
}
