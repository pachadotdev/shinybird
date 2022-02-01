# taken from Shiny 1.4.0

# buildtabset ----

containsSelectedTab <- function(tabs) {
  any(vapply(tabs, isTabSelected, logical(1)))
}

isTabSelected <- function(x) {
  isTRUE(attr(x, "selected", exact = TRUE))
}

getIcon <- function(tab = NULL, iconClass = NULL) {
  if (!is.null(tab)) iconClass <- tab$attribs$`data-icon-class`
  if (!is.null(iconClass)) {
    if (grepl("fa-", iconClass, fixed = TRUE)) {
      # for font-awesome we specify fixed-width
      iconClass <- paste(iconClass, "fa-fw")
    }
    icon(name = NULL, class = iconClass)
  } else NULL
}

# Given a vector/list, return TRUE if any elements are named, FALSE otherwise.

buildTabItem <- function(index, tabsetId, foundSelected, tabs = NULL,
                         divTag = NULL, textFilter = NULL) {

  divTag <- if (!is.null(divTag)) divTag else tabs[[index]]

  if (is.character(divTag) && !is.null(textFilter)) {
    # text item: pass it to the textFilter if it exists
    liTag <- textFilter(divTag)
    divTag <- NULL

  } else if (inherits(divTag, "shiny.navbarmenu")) {
    # navbarMenu item: build the child tabset
    tabset <- buildTabset(divTag$tabs, "dropdown-menu",
                          navbarMenuTextFilter, foundSelected = foundSelected)

    # if this navbarMenu contains a selected item, mark it active
    containsSelected <- containsSelectedTab(divTag$tabs)
    liTag <- tags$li(
      class = paste0("dropdown", if (containsSelected) " active"),
      tags$a(href = "#",
             class = "dropdown-toggle", `data-toggle` = "dropdown",
             `data-value` = divTag$menuName,
             getIcon(iconClass = divTag$iconClass),
             divTag$title, tags$b(class = "caret")
      ),
      tabset$navList   # inner tabPanels items
    )
    # list of tab content divs from the child tabset
    divTag <- tabset$content$children

  } else {
    # tabPanel item: create the tab's liTag and divTag
    tabId <- paste("tab", tabsetId, index, sep = "-")
    liTag <- tags$li(
      tags$a(
        href = paste("#", tabId, sep = ""),
        `data-toggle` = "tab",
        `data-value` = divTag$attribs$`data-value`,
        getIcon(iconClass = divTag$attribs$`data-icon-class`),
        divTag$attribs$title
      )
    )
    # if this tabPanel is selected item, mark it active
    if (isTabSelected(divTag)) {
      liTag$attribs$class <- "active"
      divTag$attribs$class <- "tab-pane active"
    }
    divTag$attribs$id <- tabId
    divTag$attribs$title <- NULL
  }
  return(list(liTag = liTag, divTag = divTag))
}

anyNamed <- function(x) {
  # Zero-length vector
  if (length(x) == 0) return(FALSE)

  nms <- names(x)

  # List with no name attribute
  if (is.null(nms)) return(FALSE)

  # List with name attribute; check for any ""
  any(nzchar(nms))
}


markTabAsSelected <- function(x) {
  attr(x, "selected") <- TRUE
  x
}

findAndMarkSelectedTab <- function(tabs, selected, foundSelected) {
  tabs <- lapply(tabs, function(div) {
    if (foundSelected || is.character(div)) {
      # Strings are not selectable items

    } else if (inherits(div, "shiny.navbarmenu")) {
      # Recur for navbarMenus
      res <- findAndMarkSelectedTab(div$tabs, selected, foundSelected)
      div$tabs <- res$tabs
      foundSelected <<- res$foundSelected

    } else {
      # Base case: regular tab item. If the `selected` argument is
      # provided, check for a match in the existing tabs; else,
      # mark first available item as selected
      if (is.null(selected)) {
        foundSelected <<- TRUE
        div <- markTabAsSelected(div)
      } else {
        tabValue <- div$attribs$`data-value` %OR% div$attribs$title
        if (identical(selected, tabValue)) {
          foundSelected <<- TRUE
          div <- markTabAsSelected(div)
        }
      }
    }
    return(div)
  })
  return(list(tabs = tabs, foundSelected = foundSelected))
}

buildTabset <- buildTabset <- function(tabs, ulClass, textFilter = NULL, id = NULL,
                                       selected = NULL, foundSelected = FALSE) {

  res <- findAndMarkSelectedTab(tabs, selected, foundSelected)
  tabs <- res$tabs
  foundSelected <- res$foundSelected

  # add input class if we have an id
  if (!is.null(id)) ulClass <- paste(ulClass, "shiny-tab-input")

  if (anyNamed(tabs)) {
    nms <- names(tabs)
    nms <- nms[nzchar(nms)]
    stop("Tabs should all be unnamed arguments, but some are named: ",
         paste(nms, collapse = ", "))
  }

  tabsetId <- p_randomInt(1000, 10000)
  tabs <- lapply(seq_len(length(tabs)), buildTabItem,
                 tabsetId = tabsetId, foundSelected = foundSelected,
                 tabs = tabs, textFilter = textFilter)

  tabNavList <- tags$ul(class = ulClass, id = id,
                        `data-tabsetid` = tabsetId, lapply(tabs, "[[", 1))

  tabContent <- tags$div(class = "tab-content",
                         `data-tabsetid` = tabsetId, lapply(tabs, "[[", 2))

  list(navList = tabNavList, content = tabContent)
}

# p_randomint ----

p_randomInt <- getFromNamespace("p_randomInt", "shiny")
