shinyUI(
  birdPage(id="mainnav", "sdcMicro GUI", theme = "shinybird.min.css",
    tabPanel("Tab 1",
             frontPage(
               title = "My Organization's Dashboard",
               subtitle = "Much content, very nice",
               background = "pattern-triangle.png"
             )
            ),
    tabPanel("Tab 2"),
    navbarMenu(
      "Tab 2",
      tabPanel(
        h1("contents"),
        h2("goes here")
      ),
      tabPanel(
        h1("contents"),
        h2("goes here")
      ),
      tabPanel(
        h1("contents"),
        h2("goes here")
      ),
      tabPanel(
        h1("contents"),
        h2("goes here")
      )
    )
  )
)
