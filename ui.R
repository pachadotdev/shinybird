shinyUI(
  suitPage("My Organization's Dashboard",
    head = suitHead(
      ## Place suit_header inside a container-fluid for correct positioning
      suitHeader(logo = "logo.svg",
                 href = "http://www.duckduckgo.com/",
                 caption = "Caption for the logo."),
      color = color
    ),
    
    tabPanel(
      "Start",
      frontPage(
         frontTitle("My Organization's Dashboard", "Much content, very nice", color = color),
         
         introTitle("Foo Bar", color = color),
         
         tags$p("This is a paragraph."),
         tags$p("This is another paragraph."),
         
         div(
            class = "box-con",
            tags$a(
               target = "_blank",
               href = "http://duckduckgo.com",
               suitBoxSmall(title = "Find out more",
                            text = "much more content")
            ),
            tags$a(
               target = "_blank",
               href = "http://duckduckgo.com",
               suitBoxLarge(title = "Find out more",
                       text = "much more content", 
                       image = "logo.svg")
            )
         ),
         
         div(
            class = "box box-timeout",
            tags$p(
              boldText("PLEASE NOTE:"),
               "This app may time-out if left idle too long, which will cause the screen to grey-out.",
               "To use the app again, refresh the page. This will reset all previously-selected input options."
            )
         )
      )
    ),
    
    navbarMenu(
      "Tab 1",
      tabPanelWithTitle("Subject 1", "all about subject 1", color = color,
             h1("contents"),
             h2("goes here")
             ), 
      tabPanelWithTitle("Subject 2", "all about subject 2", color = color,
             h1("contents"),
             h2("goes here")
             ),
      tabPanelWithTitle("Subject 3", "all about subject 3", color = color,
             h1("contents"),
             h2("goes here")
             ),
      tabPanelWithTitle("Subject 4", "all about subject 4", color = color,
             h1("contents"),
             h2("goes here")
             )
    ),
    
    navbarMenu(
      "Tab 2",
      tabPanelWithTitle("Subject 1", "all about subject 1", color = color,
             h1("contents"),
             h2("goes here")
      ), 
      tabPanelWithTitle("Subject 2", "all about subject 2", color = color,
             h1("contents"),
             h2("goes here")
      ),
      tabPanelWithTitle("Subject 3", "all about subject 3", color = color,
             h1("contents"),
             h2("goes here")
      ),
      tabPanelWithTitle("Subject 4", "all about subject 4", color = color,
             h1("contents"),
             h2("goes here")
      )
    )
  )
)
