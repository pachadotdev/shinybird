shinyUI(
  birdPage(id="mainnav", "sdcMicro GUI", theme = "shinybird.min.css", color = "blue",
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
      tabPanelWithTitle(
        "contents",
        "goes here"
      ),
      tabPanelWithTitle(
        "contents",
        "goes here"
      ),
      tabPanelWithTitle(
        "contents",
        "goes here"
      ),
      tabPanelWithTitle(
        "contents",
        "goes here"
      )
    )
  )
)
