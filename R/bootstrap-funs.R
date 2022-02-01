#' @export
card <- function (..., title = NULL, subtitle = NULL, width = 6) {
  width <- (100 / 12) * width

  titleTag <- NULL
  if (!is.null(title)) {
    titleTag <- h3(class = "card-title", title)
  }

  subtitleTag <- NULL
  if (!is.null(subtitle)) {
    subtitleTag <- h4(class = "card-subtitle", style = "color:#777777", subtitle)
  }

  div(class = "card", style = paste0("width:", width, "%"),
      div(class = "card-body",
          titleTag,
          subtitleTag,
          ...
      ),
  )
}
