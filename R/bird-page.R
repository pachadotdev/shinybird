#' Create a page with a top level navigation bar
#'
#' Create a page that contains a top level navigation bar that can be used to
#' toggle a set of [tabPanel()] or [tabPanelWithTitle()] elements.
#'
#' @param title The title to display in the app
#' @param ... [tabPanel()] elements to include in the page. The
#'   `navbarMenu` function also accepts strings, which will be used as menu
#'   section headers. If the string is a set of dashes like `"----"` a
#'   horizontal separator will be displayed in the menu.
#' @param id If provided, you can use `input$`*`id`* in your
#'   server logic to determine which of the current tabs is active. The value
#'   will correspond to the `value` argument that is passed to
#'   [tabPanel()].
#' @param selected The `value` (or, if none was supplied, the `title`)
#'   of the tab that should be selected by default. If `NULL`, the first
#'   tab will be selected.
#' @param position Determines whether the navbar should be displayed at the top
#'   of the page with normal scrolling behavior (`"static-top"`), pinned at
#'   the top (`"fixed-top"`), or pinned at the bottom
#'   (`"fixed-bottom"`). Note that using `"fixed-top"` or
#'   `"fixed-bottom"` will cause the navbar to overlay your body content,
#'   unless you add padding, e.g.: \code{tags$style(type="text/css", "body
#'   {padding-top: 70px;}")}
#' @param header Tag or list of tags to display as a common header above all
#'   tabPanels.
#' @param footer Tag or list of tags to display as a common footer below all
#'   tabPanels
#' @param inverse `TRUE` to use a dark background and light text for the
#'   navigation bar
#' @param collapsible `TRUE` to automatically collapse the navigation
#'   elements into a menu when the width of the browser is less than 940 pixels
#'   (useful for viewing on smaller touchscreen device)
#' @param fluid `TRUE` to use a fluid layout. `FALSE` to use a fixed
#'   layout.
#' @param responsive This option is deprecated; it is no longer optional with
#'   Bootstrap 3.
#' @param windowTitle The title that should be displayed by the browser window.
#'   Useful if `title` is not a string.
#' @param color Optional color for the app. It can be red (default), blue ... or
#'   a valid hex color such as #e0245e.
#' @param font Optional Google Font for a quick customization.
#' @param theme Optional CSS file within the 'www' directory.
#'
#' @examples
#' birdPage(id = "Doge", "Very Shiny")
#'
#' @importFrom utils getFromNamespace
#' @importFrom shiny bootstrapPage restoreInput tagAppendChild tabPanel span icon includeCSS
#'
#' @export
birdPage <- function(title,
                       ...,
                       id = NULL,
                       selected = NULL,
                       position = c("static-top", "fixed-top", "fixed-bottom"),
                       header = NULL,
                       footer = NULL,
                       inverse = FALSE,
                       collapsible = FALSE,
                       fluid = TRUE,
                       responsive = NULL,
                       windowTitle = title,
                       color = "blue",
                       font = NULL,
                       theme = NULL) {

  default_colors <- c("pink", "green", "blue", "yellow", "red", "purple",
                      "azure", "indigo", "orange", "lime", "teal", "cyan")

  if (!(color %in% default_colors) &
      !(substr(color, 1, 1) == "#" & nchar(color) <= 7)) {
    warning("The chosen color is not a valid color and blue will be used.")
    color <- "blue"
  }

  if (color %in% default_colors) {
    color <- switch(color,
                    "pink" = "#ce3b6d",
                    "green" = "#2ea578",
                    "blue" = "#4369c4",
                    "yellow" = "#ce3b6d",
                    "red" = "#ce3b6d",
                    "purple" = "#ae40c9",
                    "azure" = "#5e97e0",
                    "indigo" = "#5e61ea",
                    "orange" = "#ec6c15",
                    "lime" = "#73b819",
                    "teal" = "#2ea578",
                    "cyan" = "#40a1b8"
    )
  }

  # alias title so we can avoid conflicts w/ title in withTags
  pageTitle <- title

  # navbar class based on options
  navbarClass <- "navbar navbar-default"
  position <- match.arg(position)
  if (!is.null(position))
    navbarClass <- paste(navbarClass, " navbar-", position, sep = "")
  if (inverse)
    navbarClass <- paste(navbarClass, "navbar-inverse")

  if (!is.null(id))
    selected <- restoreInput(id = id, default = selected)

  # build the tabset
  tabs <- list(...)
  tabset <- buildTabset(tabs, "nav navbar-nav", NULL, id, selected)

  # function to return plain or fluid class name
  className <- function(name) {
    if (fluid)
      paste(name, "-fluid", sep="")
    else
      name
  }

  # built the container div dynamically to support optional collapsibility
  if (collapsible) {
    navId <- paste("navbar-collapse-", p_randomInt(1000, 10000), sep="")
    containerDiv <- div(class=className("container"),
                        div(class="navbar-header",
                            tags$button(type="button", class="navbar-toggle collapsed",
                                        `data-toggle`="collapse", `data-target`=paste0("#", navId),
                                        span(class="sr-only", "Toggle navigation"),
                                        span(class="icon-bar"),
                                        span(class="icon-bar"),
                                        span(class="icon-bar")
                            ),
                            span(class="navbar-brand", pageTitle)
                        ),
                        div(class="navbar-collapse collapse", id=navId, tabset$navList)
    )
  } else {
    containerDiv <- div(class=className("container"),
                        div(class="navbar-header",
                            span(class="navbar-brand", pageTitle)
                        ),
                        tabset$navList
    )
  }

  # build the main tab content div
  contentDiv <- div(class=className("container"))
  if (!is.null(header))
    contentDiv <- tagAppendChild(contentDiv, div(class="row", header))
  contentDiv <- tagAppendChild(contentDiv, tabset$content)
  if (!is.null(footer))
    contentDiv <- tagAppendChild(contentDiv, div(class="row", footer))

  # build the page
  bootstrapPage(
    title = windowTitle,
    responsive = responsive,
    tags$nav(class=navbarClass, role="navigation", containerDiv),
    tags$head(
      if (is.null(theme)) {
        includeCSS(system.file("shinybird.min.css", package = "shinybird"))
      } else {
        includeCSS(paste0("www/", theme))
      }
    ),
    if (color != "#4369c4") {
      tags$head(tags$style(
        HTML(sprintf(
          ".navbar-default .navbar-nav>.active>a,
          .navbar-default .navbar-nav>.active>a:hover,
          .navbar-default .navbar-nav>.active>a:focus {
            border-bottom: solid 2px %s !important;
          }
          div.front-page>div.img-front-page {
            background-color: %s !important;
          }",
          color, color
        ))
      ))
    },
    if (!is.null(font)) {
      tags$head(tags$style(
        HTML(sprintf(
          "
          @import url('https://fonts.googleapis.com/css2?family=%s&display=swap');
          body {
           font-family: %s,sans-serif !important;}
          h1, h2, h3, h4, h5, h6, p, a, text,
          .navbar-brand, .h1, .h2, .h3, .h4, .h5, .h6, .p, .a, .text {
           font-family: %s,sans-serif !important;}",
          gsub(" ", "+", font), font, font
        ))
      ))
    },
    contentDiv
  )
}
