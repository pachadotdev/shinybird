shinyUI(
  birdPage(id="mainnav", "Doge Demo", color = "red",
    tabPanel("Tab 1",
             frontPage(
               title = "My Organization's Dashboard",
               subtitle = "Much content, very nice",
               background = "pattern-triangle.png",
               background_scale = F
             )
            ),
    tabPanelWithTitle(
      "Tab2",
      "insert content"
    ),
    navbarMenu(
      "Tab 2",
      tabPanelWithTitle(
        "contents",
        "goes here"
      ),
      tabPanelWithTitle(
        "contents",
        "goes and here"
      ),
      tabPanelWithTitle(
        "contents",
        "also here"
      ),
      tabPanelWithTitle(
        "contents",
        "and here"
      )
    )
  )
)
