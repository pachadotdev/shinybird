shinyServer(function(session, input, output) {
  bar_chart_1 <- hchart(dstype, "bar",
                        hcaes(x = type_1, y = n, color = color_1),
                        name = "Pokemons") %>%
    hc_xAxis(title = list(text = "Type")) %>%
    hc_yAxis(title = list(text = "Amount"))

  bar_chart_2 <- hchart(dstype2, "bar",
                        hcaes(x = type_2, y = n, color = color_2),
                        name = "Pokemons") %>%
    hc_xAxis(title = list(text = "Type")) %>%
    hc_yAxis(title = list(text = "Amount"))

  tree_map_1 <- hctreemap2(
    dtm,
    group_vars = c("type_1"),
    size_var = "n",
    layoutAlgorithm = "squarified",
    levelIsConstant = FALSE,
    levels = list(
      list(level = 1, dataLabels = list(enabled = TRUE), borderWidth = 3)
    )
  )

  tree_map_2 <- tree_map_1

  tree_map_2$x$hc_opts$series[[1]]$data <- tree_map_2$x$hc_opts$series[[1]]$data %>%
    map(function(x){
      if(x$level == 1) {
        x$color <- dtm %>%
          filter(type_1 == x$type_1) %>%
          pull(color_1)
      }
      x
    })

  tree_map_2$x$hc_opts$colorAxis <- NULL

  tree_map_2 <- tree_map_2 %>%
    hc_plotOptions(
      series = list(
        dataLabels = list(
          style = list(textOutline = FALSE)
        )
      )
    ) %>%
    # this is to remove the hover effect by the default color
    hc_colors("transparent")

  output$bar_chart_1 <- renderHighchart2(bar_chart_1)
  output$bar_chart_2 <- renderHighchart2(bar_chart_2)

  output$tree_map_1 <- renderHighchart2(tree_map_1)
  output$tree_map_2 <- renderHighchart2(tree_map_2)
})
