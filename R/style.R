#' Bold Text
#'
#' @param text complete
#'
#' @return a tab panel for Shiny
#' @importFrom shiny HTML
#' @export
boldText <- function(text) {
  HTML(sprintf("<b>%s</b>", text))
}

#' Italic Text
#'
#' @param text complete
#'
#' @return a tab panel for Shiny
#' @importFrom shiny HTML
#' @export
italicText <- function(text) {
  HTML(sprintf("<i>%s</i>", text))
}

#' Tints of User Defined Color
#'
#' @param color complete
#' @param pct complete
#'
#' @return tint of specified color in hex format
#' @importFrom grDevices col2rgb rgb
#' @keywords internal
generateColors <- function(color, pct) {
  color <- (pct * (col2rgb(color) / 255)) + ((1 - pct) * c(1,1,1))
  return(rgb(color[1], color[2], color[3]))
}
