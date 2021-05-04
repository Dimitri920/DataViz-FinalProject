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
    )),
    
  dashboardBody(
    fluidRow(
      verbatimTextOutput(outputId = "text"),
      box(DTOutput(outputId = "top_likes"), width = 12, title = "Top 10 Videos in % of Likes For the Region")),
    fluidRow(
      box(DTOutput(outputId = "top_dislikes"), width = 12, title = "Top 10 Videos in % of Dislikes For the Region")),
    fluidRow(
      box(DTOutput(outputId = "top_comments"), width= 12, title = "Top 10 Videos in % of Comments For the Region")),
    
    fluidRow(
      dataTableOutput(outputId = "dt_found"))
    
    
  )
)
