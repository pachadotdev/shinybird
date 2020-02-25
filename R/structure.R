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
      tabDesc(description),
      div(class = "tabTitlePanel-end")
    ),
    ...
  )
}

#' Page With a Suit Layout
#'
#' @param title text string for the tab title
#' @param ... additional arguments
#' @param id page id
#' @param head complete
#' @param header complete
#' @param footer complete
#' @param windowTitle complete
#'
#' @return a tab panel for Shiny
#' @importFrom shiny div span tagAppendChild bootstrapPage tags
#' @importFrom utils getFromNamespace
#' @export
suitPage <- function(title, ..., id = "dashboard", head = NULL, header = NULL, footer = NULL, windowTitle = title) {
  pageTitle <- title
  tabs <- list(...)

  # buildTabset is an internal function from shiny
  buildTabset <- getFromNamespace("buildTabset", "shiny")
  tabset <- buildTabset(tabs, "nav navbar-nav", NULL, id)

  containerDiv <- div(class = "container", div(
    class = "navbar-header",
    span(class = "navbar-brand", pageTitle)
  ), tabset$navList)

  contentDiv <- div(class = "container-fluid")

  if (!is.null(header)) {
    contentDiv <- tagAppendChild(contentDiv, div(class = "row", header))
  }
  contentDiv <- tagAppendChild(contentDiv, tabset$content)

  if (!is.null(footer)) {
    contentDiv <- tagAppendChild(contentDiv, div(class = "row", footer))
  }

  bootstrapPage(
    title = windowTitle,
    head,
    tags$nav(class = "navbar navbar-default", role = "navigation", containerDiv),
    contentDiv
  )
}

#' Suit Layout Header
#'
#' @param logo URL or file name inside www/ for the navbar logo
#' @param href link for the navbar logo
#' @param caption alternative text for the navbar logo
#'
#' @return a tab panel for Shiny
#' @importFrom shiny div tags
#' @export
suitHeader <- function(logo = NULL, href = NULL, caption = NULL) {
  div(
    class = "container-fluid",
    div(
      id = "suit-header",
      div(
        class = "suit-brand",
        if (!is.null(logo)) {
          tags$a(
            class = "suit-brand",
            href = href,
            title = caption,
            tags$img(
              src = logo,
              alt = caption
            )
          )
        } else {
          NULL
        }
      ),
      tags$script('if(window != window.parent){$("#suit-header").css("display", "none");}')
    )
  )
}

#' Suit Layout Head
#'
#' @param ... additional arguments
#' @param color text string with red/green/blue or a valid hex color, unvalid colors default to red
#'
#' @return a tab panel for Shiny
#' @importFrom rlang dots_list
#' @importFrom shiny tags includeCSS HTML
#' @export
suitHead <- function(..., color = "red") {
  if (!any(color %in% c("red", "green", "blue")) | !(substr(color, 1, 1) == "#" & nchar(color) == 7)) {
    color <-  "red"
  }

  if (any(color %in% c("red", "green", "blue"))) {
    color <- switch(color,
      "red" = "#e44c65", "green" = "#05878a", "blue" = "#0074cc"
    )
    color_tint_0 <- switch(color,
      "#e44c65" = "#f6c9d0", "#05878a" = "#b4dbdb", "#0074cc" = "#b2d5ef"
    )
    color_tint_1 <- switch(color,
      "#e44c65" = "#f4b7c1", "#05878a" = "#9bcfd0", "#0074cc" = "#99c7ea"
    )
    color_tint_2 <- switch(color,
      "#e44c65" = "#f1a5b2", "#05878a" = "#82c3c4", "#0074cc" = "#7fb9e5"
    )
  } else {
    if (substr(color, 1, 1) == "#" & nchar(color) == 7) {
      pct_color_0 <- 0.55
      pct_color_1 <- 0.45
      pct_color_2 <- 0.35

      color_tint_0 <- generateColors(color, pct_color_0)
      color_tint_1 <- generateColors(color, pct_color_1)
      color_tint_2 <- generateColors(color, pct_color_2)
    }
  }
  lst <- dots_list(
    tags$head(
      includeCSS(system.file("shinysuit-styles.min.css", package = "shinysuit")),
      tags$head(tags$style(
        HTML(sprintf(
          ".navbar-default .navbar-nav>.active>a,
               .navbar-default .navbar-nav>.active>a:focus,
               .navbar-default .navbar-nav>.active>a:hover {
                 background-color: transparent;
                 box-shadow: inset 0 3px 0 0 %s !important;
               }
               div.box-large {
                background-color: %s !important;
               }
               div.box-large:hover {
                background-color: %s !important;
               }
               div.box-small {
                background-color: %s !important;
               }
               div.box-small:hover {
                background-color: %s !important;
               }
               .intro-divider {
                background: transparent linear-gradient(to right, %s 0%%, %s 100%%) repeat scroll 0%% 0%% !important;
               }
               div.tabTitlePanel-end {
                background: transparent linear-gradient(to right, %s 0%%, %s 100%%) repeat scroll 0%% 0%% !important;
               }
               div.front-banner>div.imgcon {
                background-color: %s !important;
               }",
          color, color_tint_1, color_tint_2, color_tint_0, color_tint_1, color, color_tint_1, color, color_tint_1, color
        )),
        HTML(sprintf(
          "div.front-banner>div.imgcon {
             background-image: url('%s') !important;
           }",
          "pattern-triangle.png"
        ))
      ))
    ),
    ...
  )
  class(lst) <- c("shiny.tag.list", "list")
  return(lst)
}

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
    class = "front-banner",
    div(class = "imgcon"),
    div(class = "hcon", h1(title)),
    if (!is.null(subtitle)) div(class = "hcon", h2(subtitle)) else NULL
  )
}

#' Suit Layout Introduction Title
#'
#' @param title text string for the tab title
#'
#' @return a tab panel for Shiny
#' @importFrom shiny div tags
#' @export
introTitle <- function(title) {
  div(
    tags$h3(class = "intro", title),
    div(class = "intro-divider")
  )
}

#' Small Box for Suit Page
#'
#' @param title complete
#' @param text complete
#' @param ... additional parameters
#' @param color complete
#'
#' @return a tab panel for Shiny
#' @importFrom shiny div tags
#' @export
suitBoxSmall <- function(title, text, ..., color = "red") {
  div(
    class = "float box box-small",
    tags$p(class = "intro", title),
    tags$p(text),
    ...
  )
}

#' Large Box for Suit Page
#'
#' @param title complete
#' @param text complete
#' @param image complete
#' @param ... additional parameters
#' @param color complete
#'
#' @return a tab panel for Shiny
#' @importFrom shiny div tags
#' @export
suitBoxLarge <- function(title, text, image = NULL, ..., color = "red") {
  div(
    class = "float box box-large",
    div(
      class = "box-large left-side",
      tags$p(class = "intro", title),
      tags$p(text),
      ...
    ),
    div(
      class = "box-large right-side",
      if (!is.null(image)) tags$img(class = "rear-preview", src = image) else NULL
    )
  )
}
