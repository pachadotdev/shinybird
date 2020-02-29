#' Create a Styled Panel With Titles
#'
#' Create a panel that works well with dropdowns and adds title and subtitle.
#'
#' @param title text string for the tab title
#' @param description text string for the tab description
#' @param ... additional arguments
#'
#' @examples
#' tabPanelWithTitle("Wow", "Such data")
#'
#' @importFrom shiny tabPanel div
#'
#' @export
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
