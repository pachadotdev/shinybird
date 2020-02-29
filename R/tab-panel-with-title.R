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
tabPanelWithTitle <- function(title, description = NULL, ...) {
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
