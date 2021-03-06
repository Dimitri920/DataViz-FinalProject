library(stats)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinythemes)
library(readr)
library(dplyr)
library(maps)
library(tidyverse)
library(leaflet)
library(DT)
library(tidyr)
library(stringr)


ui <- bootstrapPage(
  
  
  navbarPage("Best Youtube Videos", theme = shinytheme("flatly"), collapsible = TRUE, id = "nav",
             
             tabPanel("Interactive map",
                      div(class="outer",
                          leafletOutput("map", width = "auto", height = "1000px"),
                          
                          absolutePanel(draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                        width = 750, height = "auto",
                                        
                                        h2("Top Youtube Vids Map"),
                                        
                                        fluidRow("Click on Youtube Icon to view Average Youtube Stats per Country"),
                                        fluidRow(),
                                        selectInput("country", "Select a Country ", choices =  list("All countries","Canada","France", "Germany", "India", "Japan","Mexico","South Korea","Russia","United Kingdom", "United States" )),
                                        radioButtons("col","Select Input",
                                                     choices = c("Top Views","Top Likes","Top Dislikes"),
                                                     selected = "Top Views"),
                                        DT::dataTableOutput("top")
                          ))),
             
             tabPanel("YouTube Explorer", shinytheme("cosmo"),
                      dashboardSidebar(
                        h2("Youtube Search Engine"),
                        
                        selectInput(inputId = "region", 
                                    label = "Choose Regions", 
                                    choices = region_labels, 
                                    multiple = TRUE
                        ),
                        
                        sidebarSearchForm(textId = "titlesearch",
                                          buttonId = "searchButton",
                                          label = "Search YouTube",
                                          icon = shiny::icon("search")
                        ))),
             
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
)