library(shiny)

## Creates an appropriately styled title
tabTitle <- function(x) { 
  h3(class = "tabTitle", x)
}

## Creates an appropriately styled description
## If NA, returns NULL
tabDesc <- function(x) { 
  if (!is.na(x)) tags$p(class = "tabDesc", x) else NULL
}

## tabPanel with Title (using tabTitle)
## Also searches for a match in `tabdesc`
##  (a named vector of descriptions found in "ui_doctabs.R")
## If one is found, adds the description below the title
tabPwT <- function(title, description, ...) {
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

## Modified navbarPage from shiny
## Cuts bloat and enables use of tags$head with `thead`
suitPage <- function(title, ..., id = "dashboard", thead = NULL, header = NULL, footer = NULL, windowTitle = title) {
    pageTitle <- title
    navbarClass <- "navbar navbar-default"
    tabs <- list(...)
    tabset <- shiny:::buildTabset(tabs, "nav navbar-nav", NULL, id)
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
      title = windowTitle, thead,
      tags$nav(class = navbarClass, role = "navigation", containerDiv),
      contentDiv
    )
  }

## Adds a basic suit header, that will only display
##  if the page is not embedded as an iframe.
suit_header <- function(logo = "logo.svg") {
  div(
      id = "suit-header",
      div(
        class = "suit-brand",
        tags$a(
          class = "suit-brand", href = "http://www.duckduckgo.com/",
          title = "Caption for the logo.",
          tags$img(
            src = logo,
            alt = "Caption for the logo."
          )
        )
      ),
      tags$script('if(window != window.parent){$("#suit-header").css("display", "none");}')
    )
}
