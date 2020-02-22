# shared load ---
library(shiny)

mbie.cols2 <-
  ## mbie.cols that supports more than 7 colours
  function(x = 1:7) {
    MBIE.cols <- structure(
      c("#006272", "#97D700", "#00B5E2", "#753BBD", "#DF1995", "#FF6900", "#FBE122"),
      .Names = c("Teal", "Green", "Blue", "Purple", "Pink", "Orange", "Yellow")
    )
    if (x[1] == "Duo1") {
      x <- 1:2
    }
    if (x[1] == "Trio1") {
      x <- 1:3
    }
    if (x[1] == "Duo2") {
      x <- 2:3
    }
    if (x[1] == "Trio2") {
      x <- 3:5
    }
    if (x[1] == "Duo3") {
      x <- 4:5
    }
    if (x[1] == "Trio3") {
      x <- c(4, 6:7)
    }
    if (x[1] == "Duo4") {
      x <- 6:7
    }
    if (x[1] == "Duo5") {
      x <- c(4, 7)
    }
    if (max(x) <= 7 || mode(x) == "character") {
      as.vector(MBIE.cols[x])
    } else {
      colorRampPalette(MBIE.cols)(max(x))[x]
    }
  }

# Create a vector of the Ministry of Tourism colours
Tourism.cols <- c(
  Forest = "#3D4721FF",
  Grass = "#A8B50AFF",
  Southerly = "#5C788FFF",
  Volcano = "#B09E0DFF",
  Koura = "#D4470FFF",
  Merino = "#C2C4A3FF",
  Moss = "#5E7803FF",
  Pohutukawa = "#AD2624FF",
  Sunrise = "#D48500FF",
  CityLights = "#EDBD3DFF",
  SouthernCross = "#265787FF",
  WineCountry = "#61384DFF",
  Flax = "#708270FF",
  Ocean = "#A8C4C4FF",
  RiverStone = "#ADABA6FF",
  Waka = "#826E59FF",
  CabbageTree = "#A8AD70FF",
  Sky = "#94B5E0FF"
)

# A function tourism.cols() for easy reference to the vector Tourism.cols
tourism.cols <- function(x = c(1, 2, 3, 5, 12, 6)) {
  # function to return in vector format a subset of the Ministry of Tourism's 2007
  # palette of colors.  By default returns 6 colours that form a nice set
  # for most plots.  Note that normally the 4th colour (Volcano) is too similar
  # to the 2nd (Grass) for use in plots.
  if (x[1] == "Primary") x <- 1:6
  if (x[1] == "Supporting") x <- 7:18
  if (x[1] == "All") x <- 1:18
  if (x[1] == "Pale") x <- 13:18
  if (x[1] == "Alternating") x <- rep(c(1, 6, 4, 8, 2, 16, 3, 5, 15, 18, 9, 14, 7, 12, 11, 17, 10, 13), 6)
  as.vector(Tourism.cols[x])
}

tabTitle <-
  ## Creates an appropriately styled title
  function(x)
    h3(class = "tabTitle", x)

tabDesc <-
  ## Creates an appropriately styled description
  ## If NA, returns NULL
  function(x)
    if (!is.na(x)) tags$p(class = "tabDesc", x) else NULL

tabPwT <-
  ## tabPanel with Title (using tabTitle)
  ## Also searches for a match in `tabdesc`
  ##  (a named vector of descriptions found in "ui_doctabs.R")
  ## If one is found, adds the description below the title
  function(title, description, ...) {
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
dashboardPage <-
  function(title, ..., id = "dashboard", thead = NULL, header = NULL, footer = NULL, windowTitle = title) {
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

mbie_header <-
  ## Adds a basic MBIE header, that will only display
  ##  if the page is not embedded as an iframe.
  ## Requires: www/mbie-logo.png
  function() div(
      id = "mbie-header",
      div(class = "mbie-topbar"),
      div(
        class = "mbie-brand",
        tags$a(
          class = "mbie-brand", href = "http://www.mbie.govt.nz/",
          title = "Ministry of Business, Innovation & Employment home page.",
          tags$img(
            src = "logo.png",
            alt = "Ministry of Business, Innovation & Employment"
          )
        )
      ),
      tags$script('if(window != window.parent){$("#mbie-header").css("display", "none");}')
    )
