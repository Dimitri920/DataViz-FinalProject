library(stats)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinythemes)
library(readr)
library(dplyr)
library(maps)
library(leaflet)
library(DT)
library(tidyr)
library(stringr)

youtubeglobal <- read_csv("YoutubeGlobalData.csv")



colnames(youtubeglobal)[18] <- "country"
youtubeglobal

youtubeglobal$country[ youtubeglobal$country == 'Great Britian'] <- "United Kingdom"

youtubeglobal$country[ youtubeglobal$country == 'Great Britain'] <- "United Kingdom"

youtube.summary <- youtubeglobal %>%
    select(views, likes, comment_count,country) %>%
    group_by(country) %>% 
    summarise(views=mean(views), likes = mean(likes), comment_count = mean(comment_count))
youtube.summary[,-1]<- round(youtube.summary[,-1],0) 

world.cities$country.etc[ world.cities$country.etc == 'USA'] <- "United States"

world.cities$country.etc[ world.cities$country.etc == 'UK'] <- "United Kingdom"

world.cities$country.etc[ world.cities$country.etc == 'Korea South'] <- "South Korea"

youtube.summary$country[ youtube.summary$country == 'Great Britain'] <- "United Kingdom"

# merge data with world country data to get geolocation 
youtube.summary

youtube.newsum <- world.cities %>%
    filter(capital == 1) %>%
    dplyr::select(country = country.etc, lat, lng = long) %>%
    left_join(youtube.summary, by = "country")




#Omit na's to reveal countries we want 


youmapsum<-youtube.newsum %>% na.omit()

region_labels <- unique(youtubeglobal$country)


ui <- bootstrapPage(
    
    
    navbarPage("Best Youtube Videos", theme = shinytheme("flatly"), collapsible = TRUE, id = "nav",
               
               tabPanel("Interactive map",
                        div(class="outer",
                            #tags$head(type = "text/css", "html, body {width:100%;height:100%}"),
                            leafletOutput("map", width = "auto", height = "1000px"),
                            
                            absolutePanel(draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                          width = 750, height = "auto",
                                          
                                          h2("Top Youtube Videos"),
                                          
                                          fluidRow("Click on Youtube Icon to view Average Youtube Stats per Country"),
                                          fluidRow(),
                                          selectInput("country", "Select a Country ", choices =  list("All countries","Canada","France", "Germany", "India", "Japan","Mexico","South Korea","Russia","United Kingdom", "United States" )),
                                          radioButtons("col","Select Input",
                                                       choices = c("Top Views","Top Likes","Top Dislikes"),
                                                       selected = "Top Views"),
                                          DT::dataTableOutput("top")
                            ))),
               
               tabPanel("YouTube Explorer",
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

youtube_world <- youtubeglobal

colnames(youtube_world)[18] <- "region"
head(youtube_world)


youmapsum <- mutate(youmapsum,
                    popup_text=paste0("<center><br><b>",youmapsum$country,"</br></b>",
                                      "Views:", youmapsum$views, "<br>",
                                      "Likes:", youmapsum$likes, "<br>",
                                      "Comment Count:", youmapsum$comment_count)
                    
                    
)


library(stringr)
globaltube<- youtubeglobal %>% distinct(title,.keep_all = TRUE)
globaltube$country[ globaltube$country == 'Great Britain'] <- "United Kingdom"
# make empty data frame to return nothing when all countries are selected in datatable

nothing<- data.frame()

youtubeIcon <- makeIcon( iconUrl = "https://seeklogo.com/images/Y/youtube-2017-icon-logo-D1FE045118-seeklogo.com.png", iconWidth = 30, iconHeight = 25,
                         iconAnchorX = 22, iconAnchorY = 25)


server <-function(input, output, session) {
    
    ## Interactive Map ###########################################
    
    # Create the map
    mapregion <-  map("world", fill = TRUE, plot = FALSE)
    
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




shinyApp(ui, server)

