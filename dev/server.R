shinyServer(function(input, output, session) {
  d <- reactive({ filter(datasaurus,  dataset == input$filter_dataset) })

  s <- reactive({
    d() %>%
      summarise(
        mean_x = mean(x), mean_y = mean(y),
        median_x = median(x), median_y = median(y),
        sd_x = sd(x), sd_y = sd(y)
      )
      # gather(statistic, value)
  })

  p <- reactive({
    hchart(d(), "scatter", hcaes(x = x, y = y))
  })

  output$s <- renderTable({ s() })

  output$p <- renderHighchart({ p() })
})
