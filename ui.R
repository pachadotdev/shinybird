shinyUI(
  dashboardPage("New Zealand Tourism Dashboard",
    thead = tagList(
      tags$head(
        includeCSS("mbie-styles.css"),
        includeCSS("tdstyles.css"),
        tags$script(src = "jszip.min.js"),
        tags$script(src = "accounting.min.js"),
        tags$script("accounting.settings.currency.precision = 0;"),
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
        ## Disable DataTable error reporting
        tags$script('$.fn.dataTableExt.sErrMode = "throw";'),
        ## iframe resizer code to dynamically adjust iframe height
        ## also requires work by mbie web services to work
        tags$script(src = "iframeResizer.contentWindow.min.js")
      ),
      ## Place mbie_header inside a container-fluid for correct positioning
      div(class = "container-fluid", mbie_header())
    ),
    tabPanel(
      "Start",
      div(
         class = "frontp",
         div(
            class = "front-banner",
            div(class = "imgcon"),
            div(class = "hcon", h1("The New Zealand"), h1("Tourism Dashboard"))
         ),
         # tags$p(tags$span(class = "warning", "This app is still in active development and has not been officially launched.")),
         tags$p(class = "intro", "The New Zealand Tourism Dashboard is a one-stop shop for all information about tourism. It brings together a range of tourism datasets produced by MBIE and Statistics New Zealand into one easy-to-use tool. Information is presented using dynamic graphs and data tables."),
         div(class = "intro-divider"),
         tags$p("Main subject-area groupings of tourism data are shown on the toolbar above. To navigate around this site, left-click one of the subject areas and then select one of the related categories in the drop down list."),
         tags$p(
            "Graphs can be downloaded as PDFs and tables can be downloaded to a csv file. Information on how to use the graphs and tables, along with an example, is available in the",
            tags$a("Help", title = "Help Tab", href = "#", id = "HelpTabLink"), "tab.",
            tags$script("$('#HelpTabLink').click(function(){$('a[data-value=\"Help\"]')[0].click();});")
         ),
         div(
            class = "box-con",
            tags$a(
               target = "_blank",
               href = "http://mbie.govt.nz/info-services/sectors-industries/tourism/tourism-research-data/tourism-data-sources",
               div(
                  class = "float box box-more",
                  tags$p(class = "intro", "Find out more"),
                  tags$p("Click here for more information on the data sources used and their differences.")
               )
            ),
            tags$a(
               target = "_blank",
               href = "http://www.mbie.govt.nz/info-services/business/business-growth-agenda/regions",
               div(
                  class = "float box box-rear",
                  tags$p(
                     tags$img(class = "rear-preview", src = "REAR-cover-2015.jpg"),
                     "The", span(class = "bold", "Regional Economic Activity Report"),
                     "produced by MBIE presents regional information and trends that supplements the information in this dashboard. Click here to find the online tool and mobile app."
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
      "Overview",
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
