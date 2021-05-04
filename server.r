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
library(purrr)

server <-function(input, output, session) {
  
  ## Interactive Map ###########################################
  
  # Create the map
  mapregion <-  maps::map("world", fill = TRUE, plot = FALSE)
  
  filteredData <- reactive({
    if (input$country == "All countries") {
      youmapsum
    } else {
      filter(youmapsum, country == input$country)
    }})
  
  topview<- reactive({
    if(input$country == "All countries"){
      nothing
    }else{
      select(globaltube, title,views, likes,comment_count, dislikes,country)%>%
        arrange(desc(views)) %>% 
        dplyr::filter(country == input$country)
    }
  })
  
  toplike<- reactive({
    if(input$country == "All countries"){
      nothing
    }else{
      select(globaltube, title,views, likes,comment_count, dislikes,country)%>%
        arrange(desc(likes)) %>% 
        dplyr::filter(country == input$country)
    }
  })
  
  topdislikes<- reactive({
    if(input$country == "All countries"){
      nothing
    }else{
      select(globaltube, title,views, likes,comment_count, dislikes,country)%>%
        arrange(desc(dislikes)) %>% 
        dplyr::filter(country == input$country)
    }
  })
  
  
  
  output$map <- renderLeaflet({
    leaflet(filteredData()) %>% 
      #addTiles(data = mapregion) %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>% 
      addMarkers(~lng, ~lat, 
                 #data = youmapsum,  
                 icon = youtubeIcon,
                 labelOptions = labelOptions(textsize = "15px"))
  })
  
  output$top<-DT::renderDataTable({
    if(input$col == "Top Views"){
      DT::datatable(head(topview(),5),options = list(
        lengthMenu = list(c(3, 5, -1)),
        pageLength = 5,
        searching = FALSE,
        paging = FALSE,
        scrollX = TRUE))
    }
    else if(input$col == "Top Likes"){
      DT::datatable(head(toplike(),5),options = list(
        lengthMenu = list(c(3, 5, -1)),
        pageLength = 5,
        searching = FALSE,
        paging = FALSE,
        scrollX = TRUE))
    }  
    else if(input$col == "Top Dislikes"){
      DT::datatable(head(topdislikes(),5),options = list(
        lengthMenu = list(c(3, 5, -1)),
        pageLength = 5,
        searching = FALSE,
        paging = FALSE,
        scrollX = TRUE))
    }
  }) 
  
  
  
  
  
  observe({
    leafletProxy("map", data = filteredData()) %>%
      clearMarkers() %>%
      clearShapes() %>%
      addMarkers( ~lng, ~lat, 
                  label = ~country, 
                  labelOptions = labelOptions(textsize = "15px"),
                  icon = youtubeIcon,
                  popup =  ~popup_text,
                  popupOptions = popupOptions(textsize = "15px")
      )
  })
  
  
  regiondata <- reactive({
    
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
      filter(region %in% input$region)
    
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
    } else{
      dt_found <- regiondata()
    }
    return(dt_found)
    
  })
  
  
  
  
  output$top_likes <- renderDT({
    likes_df <- regiondata() %>%
      mutate(across(title, str_sub, 1, 50)) %>%
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
      mutate(across(title, str_sub, 1, 50)) %>%
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
      mutate(across(title, str_sub, 1, 50)) %>%
      distinct(video_id, .keep_all = TRUE) %>% 
      mutate("Percentage_Comments"=round(100*((comment_count)/(views)),digits = 2)) %>%
      arrange(-Percentage_Comments) %>% top_n(10,wt = Percentage_Comments) %>%
      select(title, video_id, trending_date, comment_count, views, Percentage_Comments)
    
    DT::datatable(comments_df, extensions = 'Scroller', options = list(
      deferRender = TRUE,
      scrollY = 200,
      scroller = TRUE))
  })
  
}