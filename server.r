library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(DT)

shinyServer(function(input, output, session) {
  
  regiondata <- eventReactive(input$region, {
    myregion <- input$region
    
    df <- youtube_world %>%
      filter(region==myregion)
  })
  
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
  
  output$top_likes <- renderDT({
    likes_df <- regiondata() %>%
      mutate(across(title, str_sub, 1, 10)) %>%
      distinct(video_id, .keep_all = TRUE) %>% 
      mutate("Percentage_Likes"=round(100*((likes)/(views)),digits = 2)) %>% 
      arrange(-Percentage_Likes) %>% top_n(10,wt = Percentage_Likes) %>%
      select(title, video_id, trending_date, likes, views, Percentage_Likes)
    
    DT::datatable(likes_df, extensions = 'Scroller',  options = list(
      deferRender = TRUE,
      scrollY = 200,
      scroller = TRUE))
  })
  
  output$top_dislikes <- renderDT({
    dislikes_df <- regiondata() %>%
      mutate(across(title, str_sub, 1, 10)) %>%
      distinct(video_id, .keep_all = TRUE) %>% 
      mutate("Percentage_Dislikes"=round(100*((dislikes)/(views)),digits = 2)) %>%
      arrange(-Percentage_Dislikes) %>% top_n(10,wt = Percentage_Dislikes) %>%
      select(title, video_id, trending_date, dislikes, views, Percentage_Dislikes)
    
    DT::datatable(dislikes_df, extensions = 'Scroller', options = list(
      deferRender = TRUE,
      scrollY = 200,
      scroller = TRUE))
  })
  
  output$top_comments <- renderDT({
    comments_df <- regiondata() %>%
      mutate(across(title, str_sub, 1, 10)) %>%
      distinct(video_id, .keep_all = TRUE) %>% 
      mutate("Percentage_Comments"=round(100*((comment_count)/(views)),digits = 2)) %>%
      arrange(-Percentage_Comments) %>% top_n(10,wt = Percentage_Comments) %>%
      select(title, video_id, trending_date, comment_count, views, Percentage_Comments)
    
    DT::datatable(comments_df, extensions = 'Scroller', options = list(
      deferRender = TRUE,
      scrollY = 200,
      scroller = TRUE))
  })
  
})