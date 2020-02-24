library(shiny)

color <- "#96ceb4"

## Creates an appropriately styled title
tabTitle <- function(x) { 
  h1(class = "tabTitle", x)
}

## Creates an appropriately styled description
## If NA, returns NULL
tabDesc <- function(x) { 
  if (!is.na(x)) tags$h2(class = "tabDesc", x) else NULL
}

## tabPanel with Title (using tabTitle)
## Also searches for a match in `tabdesc`
##  (a named vector of descriptions found in "ui_doctabs.R")
## If one is found, adds the description below the title
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

## Modified navbarPage from shiny
## Cuts bloat and enables use of tags$head with `thead`
suitPage <- function(title, ..., id = "dashboard", head = NULL, header = NULL, footer = NULL, windowTitle = title) {
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
      title = windowTitle, head,
      tags$nav(class = navbarClass, role = "navigation", containerDiv),
      contentDiv
    )
  }

## Adds a basic suit header, that will only display
##  if the page is not embedded as an iframe.
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

frontPage <- function(...) {
  div(
    class = "front-page",
    ...
  )
}

frontTitle <- function(title, subtitle = NULL) {
  div(
    class = "front-banner",
    div(class = "imgcon"),
    div(class = "hcon", h1(title)),
    if (!is.null(subtitle)) div(class = "hcon", h2(subtitle)) else NULL
  )
}

introTitle <- function(text) {
  div(
    tags$h3(class = "intro", text),
    div(class = "intro-divider")
  )
}

suitHead <- function(..., color = "red") {
  if (any(color %in% c("red", "green", "blue"))) {
    color <- switch(
      color,
      "red" = "#e44c65", 
      "green" = "#05878a", 
      "blue" = "#0074cc"
    )
    color_tint_0 <- switch(
      color,
      "#e44c65" = "#f6c9d0", 
      "#05878a" = "#b4dbdb", 
      "#0074cc" = "#b2d5ef"
    )
    color_tint_1 <- switch(
      color,
      "#e44c65" = "#f4b7c1", 
      "#05878a" = "#9bcfd0", 
      "#0074cc" = "#99c7ea"
    )
    color_tint_2 <- switch(
      color,
      "#e44c65" = "#f1a5b2", 
      "#05878a" = "#82c3c4", 
      "#0074cc" = "#7fb9e5"
    )
  } else {
    if (substr(color, 1, 1) == "#" & nchar(color) == 7) {
      pct_color_0 <- 0.55
      pct_color_1 <- 0.45
      pct_color_2 <- 0.35
      
      color_tint_0 <- (pct_color_0 * (col2rgb(color) / 255)) + ((1 - pct_color_0) * c(1,1,1))
      color_tint_0 <- rgb(color_tint_0[1], color_tint_0[2], color_tint_0[3])
      
      color_tint_1 <- (pct_color_1 * (col2rgb(color) / 255)) + ((1 - pct_color_1) * c(1,1,1))
      color_tint_1 <- rgb(color_tint_1[1], color_tint_1[2], color_tint_1[3])
      
      color_tint_2 <- (pct_color_2 * (col2rgb(color) / 255)) + ((1 - pct_color_2) * c(1,1,1))
      color_tint_2 <- rgb(color_tint_2[1], color_tint_2[2], color_tint_2[3])
    }
  }
  lst <- rlang::dots_list(
    tags$head(
      includeCSS("shinysuit-styles.min.css"),
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
            ))
        )),
      tags$script(src = "jquery-ui-1-11-4.min.js"),
      ## Add jQuery UI tooltips
      ## Use: have class "jui-tip" and
      ##      title attribute = tooltip message
      ## e.g. tags$div(class = "jui-tip", title = "Tooltip Message", radioButtons(...))
      tags$script('$(function(){$(".jui-tip").tooltip();});'),
      ## Use jQuery UI accordion for nice looking show/hide inputs feature
      tags$script('$(function(){$("div.divinput").accordion({
            collapsible: true,
            heightStyle: "content"
         });});'),
      ## iframe resizer code to dynamically adjust iframe height
      ## also requires work by suit web services to work
      tags$script(src = "iframeResizer.contentWindow.min.js")
    ),
    ...
  )
  class(lst) <- c("shiny.tag.list", "list")
  return(lst)
}

boldText <- function(text) {
  HTML(sprintf("<b>%s</b>", text))
}

italicText <- function(text) {
  HTML(sprintf("<i>%s</i>", text))
}

suitBoxSmall <- function(title, text, ..., color = "red") {
  div(
    class = "float box box-small",
    tags$p(class = "intro", title),
    tags$p(text),
    ...
  )
}
  
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
