shinyUI(
  suitPage("My Organization's Dashboard",
    thead = tagList(
      tags$head(
        includeCSS("shinysuit-styles.min.css"),
        tags$script(src = "jquery-ui-1-11-4.min.js"),
        ## Add jQuery UI tooltips
        ## Use: have class "jui-tip" and
        ##      title attribute = tooltip message
        ## e.g. tags$div(class = "jui-tip", title = "Tooltip Message", radioButtons(...))
        tags$script('$(function(){$(".jui-tip").tooltip();});'),
        ## Use jQuery UI accordion for nice looking show/hide inputs feature
        tags$script('$(function(){$("div.divinput").accordion({
            collapsible: true,
            heightStyle: "content"
         });});'),
        ## iframe resizer code to dynamically adjust iframe height
        ## also requires work by suit web services to work
        tags$script(src = "iframeResizer.contentWindow.min.js")
      ),
      ## Place suit_header inside a container-fluid for correct positioning
      div(class = "container-fluid", suit_header())
    ),
    tabPanel(
      "Start",
      div(
         class = "frontp",
         div(
            class = "front-banner",
            div(class = "imgcon"),
            div(class = "hcon", h1("My Organization's"), h1("Dashboard"))
         ),
         tags$p(class = "intro", "This is the intro. Much content."),
         div(class = "intro-divider"),
         tags$p("This is a paragraph."),
         tags$p("This is another paragraph."),
         div(
            class = "box-con",
            tags$a(
               target = "_blank",
               href = "http://duckduckgo.com",
               div(
                  class = "float box box-more",
                  tags$p(class = "intro", "Find out more"),
                  tags$p("More content.")
               )
            ),
            tags$a(
               target = "_blank",
               href = "http://duckduckgo.com",
               div(
                  class = "float box box-rear",
                  tags$p(
                    tags$img(class = "rear-preview", src = "REAR-cover-2015.jpg"),
                    "Here's", span(class = "bold", "much more content"),
                    "with plenty of detail."
                  )
               )
            )
         ),
         div(
            class = "box box-timeout",
            tags$p(
               tags$span(class = "bold", "PLEASE NOTE:"),
               "This app may time-out if left idle too long, which will cause the screen to grey-out.",
               "To use the app again, refresh the page. This will reset all previously-selected input options."
            )
         )
      )
    ),
    navbarMenu(
      "Tab 1",
      tabPwT("Subject 1", "all about subject 1",
             h1("contents"),
             h2("goes here")
             ), 
      tabPwT("Subject 2", "all about subject 2",
             h1("contents"),
             h2("goes here")
             ),
      tabPwT("Subject 3", "all about subject 3",
             h1("contents"),
             h2("goes here")
             ),
      tabPwT("Subject 4", "all about subject 4",
             h1("contents"),
             h2("goes here")
             )
    ),
    navbarMenu(
      "Tab 2",
      tabPwT("Subject 1", "all about subject 1",
             h1("contents"),
             h2("goes here")
      ), 
      tabPwT("Subject 2", "all about subject 2",
             h1("contents"),
             h2("goes here")
      ),
      tabPwT("Subject 3", "all about subject 3",
             h1("contents"),
             h2("goes here")
      ),
      tabPwT("Subject 4", "all about subject 4",
             h1("contents"),
             h2("goes here")
      )
    )
  )
)
