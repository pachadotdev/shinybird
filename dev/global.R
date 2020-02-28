library(shiny)

#' Suit Layout Front Page
#'
#' @param ... additional arguments
#'
#' @return a tab panel for Shiny
#' @importFrom shiny div
#' @export
frontPage <- function(...) {
  div(
    class = "front-page",
    ...
  )
}

#' Suit Layout Front Title
#'
#' @param title text string for the tab title
#' @param subtitle text string for the tab subtitle
#'
#' @return a tab panel for Shiny
#' @importFrom shiny div h1 h2
#' @export
frontTitle <- function(title, subtitle = NULL) {
  div(
    class = "front-page",
    div(class = "img-front-page"),
    div(class = "title-front-page",
        h1(title),
        if (!is.null(subtitle)) h2(subtitle) else NULL
    )
  )
}

frontBackground <- function(background_image) {
  if (!is.null(background_image)) {
    tags$head(
      tags$style(
        HTML(sprintf(
          "div.front-page>div.img-front-page { background-image: url('%s') !important; }",
          background_image
        ))
      ))
  } else {
    NULL
  }
}

#' Styled Title for Tab
#'
#' @param title text string for the tab title
#'
#' @return an html output for Shiny
#' @export
#'
#' @examples
#' tabTitle("foo bar")
tabTitle <- function(title) {
  h1(class = "tabTitle", title)
}

#' Styled Description for Tab
#'
#' @param description text string for the tab description
#'
#' @return an html output for Shiny
#' @importFrom shiny tags
#' @export
#'
#' @examples
#' tabDesc("the details about foo bar")
tabDesc <- function(description) {
  if (!is.na(description)) tags$h2(class = "tabDesc", description) else NULL
}

#' Tab Panel With Title
#'
#' @param title text string for the tab title
#' @param description text string for the tab description
#' @param ... additional arguments
#'
#' @return a tab panel for Shiny
#' @importFrom shiny tabPanel div
#' @export
#'
#' @examples
#' tabPanelWithTitle("foo bar", "the details about foo bar")
tabPanelWithTitle <- function(title, description, ...) {
  tabPanel(
    title,
    div(
      class = "tabTitlePanel",
      tabTitle(title),
      tabDesc(description)
    ),
    ...
  )
}
