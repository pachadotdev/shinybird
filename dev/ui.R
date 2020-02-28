shinyUI(
  navbarPage(id="mainnav", theme = "shinybird.min.css", "sdcMicro GUI",
    tabPanel("Tab 1",
             frontPage(
               frontTitle("My Organization's Dashboard", "Much content, very nice"),
               frontBackground("pattern-triangle.png")
             )
            ),
    tabPanel("Tab 2"),
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
    ),
    tags$head(tags$script(
      src = "www/shinybird-style.js"
    ))
  )
)
