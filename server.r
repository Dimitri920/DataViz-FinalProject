library(shiny)
library(shinydashboard)
library(ggplot2)
library(readr)
library(DT)
library(dplyr)

shinyServer(function(input, output, session) {
  
  updateSelectInput(session,
                    "region", 
                    choices = region_labels, 
                    selected = c("United States", "Great Britain", "Germany", 
                                 "Canada", "France", "Russia", "Mexico", 
                                 "South Korea", "Japan", "India"))
  
  updateSearchInput(session,
                    "titlesearch")
  
  output$text <- renderPrint({
    input$titlesearch
  })
  
  output$dt_found <- renderDataTable({
    req(input$searchButton == TRUE)
    
    if(input$titlesearch == ""){
      dt_found <- NULL
    } 
    
    else{
      txt <- input$titlesearch
      
      youtube_world %>%
        mutate(
          keep_this_row = case_when(
            grepl(txt, youtube_world$title) ~ T,
            grepl(txt, youtube_world$channel_title) ~ T,
            grepl(txt, youtube_world$tags) ~ T,
            T ~ F
          )
        ) %>%
        filter(keep_this_row == T) %>%
        filter(region %in% input$region) -> dt_found
    }
    return(dt_found)
  })
  
}
)