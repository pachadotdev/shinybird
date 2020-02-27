shinyUI(
  suitPage("My Organization's Dashboard",
    head = suitHead(
      ## Place suit_header inside a container-fluid for correct positioning
      suitHeader(
        logo = "logo.svg",
        href = "http://www.duckduckgo.com/",
        caption = "Caption for the logo."
      ),
      decoration_color = decoration_color,
      background_color = background_color,
      background_image = "pattern-triangle.png"
    ),

    tabPanel(
      "Start",
      frontPage(
        frontTitle("My Organization's Dashboard", "Much content, very nice"),

        introTitle("Foo Bar"),

        tags$p("This is a paragraph."),
        tags$p("This is another paragraph.")
      )
    ),

    navbarMenu(
      "datasauRus",
      tabPanelWithTitle(
        "Subject 1", "all about subject 1",
        column(
          width = 12,
          h1("contents"),
          h2("goes here")
        ),
        column(width = 4, selectInput("filter_dataset", "Dataset", unique(datasaurus$dataset))),
        column(width = 4, tableOutput("s")),
        column(width = 4, highchartOutput("p"))
      )
    ),

    navbarMenu(
      "Tab 2",
      tabPanelWithTitle(
        "Subject 1", "all about subject 1",
        h1("contents"),
        h2("goes here")
      ),
      tabPanelWithTitle(
        "Subject 2", "all about subject 2",
        h1("contents"),
        h2("goes here")
      ),
      tabPanelWithTitle(
        "Subject 3", "all about subject 3",
        h1("contents"),
        h2("goes here")
      ),
      tabPanelWithTitle(
        "Subject 4", "all about subject 4",
        h1("contents"),
        h2("goes here")
      )
    )
  )
)
