library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)

dashboardPage(
  dashboardHeader(title = "YouTube Search Engine"),
  dashboardSidebar(
    
    selectInput(inputId = "region", 
                label = "Choose Regions", 
                choices = region_labels, 
                multiple = TRUE
    ),
    
    sidebarSearchForm(textId = "titlesearch",
                      buttonId = "searchButton",
                      label = "Search YouTube",
                      icon = shiny::icon("search")
    ),
    
    checkboxInput(inputId = "comments",
                  label = "Show only Videos with Comments Disabeled"
    ),
    
    checkboxInput(inputId = "ratings",
                  label = "Show only Videos with Ratings Disabeled")
    
    ),
  
    dashboardBody(
      verbatimTextOutput(outputId = "text"),
      tableOutput("data"),
      dataTableOutput(outputId = "dt_found")
  )
)

