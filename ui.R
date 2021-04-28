library(shiny)
library(shinydashboard)

dashboardPage(
  dashboardHeader(title = "YouTube Search Engine"),
  dashboardSidebar(
    dateInput("dates", "Select a Date:",
                   min = "2006-07-23",
                   max = "2018-06-14"
    ),
    dashboardBody(
      # Create a table of trending variables
      box(width = 6,
          status = "warning",
          title = "Trending Topics",
          solidHeader = T,
          collapsible = T,
          renderDataTable("trending_data")
        
        
        
      )
      
      
      
      
    )
  )
)
