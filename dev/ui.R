shinyUI(
  birdPage(id="mainnav", "Pokemon Demo", color = "teal",
           background_color = "#fff",
           font = "Source Sans Pro",
    tabPanel("About",
             frontPage(
               title = "About Pokemon",
               subtitle = "Back then, they were 151",
               background = "pattern-triangle.png",
               background_scale = F
             )
            ),
    tabPanelWithTitle(
      "Pokemon by type",
      "Add longer description here",
      column(12, "Add even longer description here"),
      column(
        6,
        h1("Pokemon type 1"),
        h2("Subtitle"),
        p("More text"),
        highchartOutput("bar_chart_1")
      ),
      column(
        6,
        h1("Pokemon type 2"),
        h2("Subtitle"),
        p("More text"),
        highchartOutput("bar_chart_2")
      ),
    ),
    navbarMenu(
      "Pokemon by type (continued)",
      tabPanelWithTitle(
        "Treemap 1",
        column(12, highchartOutput("tree_map_1"))
      ),
      tabPanelWithTitle(
        "Treemap 2",
        column(12, highchartOutput("tree_map_2"))
      )
    )
  )
)
