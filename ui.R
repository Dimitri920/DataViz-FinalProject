library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)

dashboardPage(
  dashboardHeader(title = "YouTube Search Engine"),
  dashboardSidebar(
    
<<<<<<< Steven
    selectInput("region", "Select a Region:",
                choices = "United States"),
=======
    selectInput(inputId = "region", 
                label = "Choose Regions", 
                choices = region_labels, 
                multiple = TRUE
    ),
>>>>>>> staging
    
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
    
<<<<<<< Steven
  ),
  
  dashboardBody(
    fluidRow(
      verbatimTextOutput(outputId = "text"),
      tableOutput("data"),
      dataTableOutput(outputId = "dt_found")),
    
    fluidRow(
      box(DTOutput(outputId = "top_likes"), width = 12, title = "Top 10 Videos in % of Likes For the Region")),
    fluidRow(
      box(DTOutput(outputId = "top_dislikes"), width = 12, title = "Top 10 Videos in % of Dislikes For the Region")),
    fluidRow(
      box(DTOutput(outputId = "top_comments"), width= 12, title = "Top 10 Videos in % of Comments For the Region"))
=======
    ),
  
    dashboardBody(
      verbatimTextOutput(outputId = "text"),
      tableOutput("data"),
      dataTableOutput(outputId = "dt_found")
>>>>>>> staging
  )
)
