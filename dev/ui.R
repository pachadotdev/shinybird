shinyUI(
  birdPage(id="mainnav", "Pokemon Demo", color = "teal", font = "Roboto Condensed",
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
      "Add even longer description here",
      card(
        title = "Card title",
        subtitle = "Card subtitle",
        "More text but inside the card",
        highchartOutput("bar_chart_1")
      )
    ),
    navbarMenu(
      "Pokemon by type (continued)",
      tabPanelWithTitle(
        "Treemap 1",
        highchartOutput("tree_map_1")
      ),
      tabPanelWithTitle(
        "Treemap 2",
        highchartOutput("tree_map_2")
      )
    )
  )
)
