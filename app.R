

library(stats)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinythemes)
library(readr)
library(dplyr)
library(maps)
library(leaflet)
library(ggvis)


youtubeglobal <- read_csv("YoutubeGlobalData.csv")

colnames(youtubeglobal)[18] <- "country"
youtubeglobal

youtubeglobal$country[ youtubeglobal$country == 'Great Britian'] <- "United Kingdom"

youtubeglobal$country[ youtubeglobal$country == 'Great Britain'] <- "United Kingdom"



#Summary videos by region

youtube.summary <- youtubeglobal %>%
  select(views, likes, comment_count,country) %>%
    group_by(country) %>% 
  summarise(views=mean(views), likes = mean(likes), comment_count = mean(comment_count))


youtube.summary[,-1]<- round(youtube.summary[,-1],0) 

youtube.summary


# merge data with world country data to get geolocation 

youtube.summary$capitol <- c("Ottawa", "Paris", "Berlin", "London", "New Delhi", "Tokyo", "Mexico City", "Moscow", "Seoul", "Washington D.C." )

data("world.cities")


#colnames(youtube.summary)[1] <- "country"
#youtube.summary

world.cities

 world.cities$country.etc[ world.cities$country.etc == 'USA'] <- "United States"
 
world.cities$country.etc[ world.cities$country.etc == 'UK'] <- "United Kingdom"
 
world.cities$country.etc[ world.cities$country.etc == 'Korea South'] <- "South Korea"

youtube.summary$country[ youtube.summary$country == 'Great Britain'] <- "United Kingdom"

youtube.summary

youtube.newsum <- world.cities %>%
    filter(capital == 1) %>%
    dplyr::select(country = country.etc, lat, lng = long) %>%
    left_join(youtube.summary, by = "country")



youmapsum<-youtube.newsum %>% na.omit()
youmapsum


#add column with pop up data for map
youmapsum <- mutate(youmapsum,
                    popup_text=paste0("<center><br><b>",youmapsum$country,"</br></b>",
                    "Views:", youmapsum$views, "<br>",
                    "Likes:", youmapsum$likes, "<br>",
                    "Comment Count:", youmapsum$comment_count))
                    
  
#Build UI

ui <- bootstrapPage(
  
  
  navbarPage("Best Youtube Videos", theme = shinytheme("flatly"), collapsible = TRUE, id = "nav",
  
  tabPanel("Interactive map",
           div(class="outer",
  #tags$head(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "auto", height = "900px"),
  
  absolutePanel(draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
        width = 750, height = "auto",

        h2("Best Videos"),
        
        fluidRow("Click on Youtube Icon to view Average Youtube Stats per Country"),
        fluidRow(),
        selectInput("country", "Select a Country ", choices =  list("All countries","Canada","France", "Germany", "India", "Japan","Mexico","South Korea","Russia","United Kingdom", "United States" )),
        radioButtons("col","Select Input",
                   choices = c("Top Views","Top Likes","Top Dislikes"),
                   selected = "Top Views"),
        DT::dataTableOutput("top")
  ))),
  
  tabPanel("Data explorer",
    fluidRow(
      column(3,
        selectInput("country", "country", c("All Countries"="", structure(state.abb, names=state.name), "Washington, DC"="DC"), multiple=TRUE))))))
  
  
        

#filter duplicate titles

globaltube<- youtubeglobal %>% distinct(title,.keep_all = TRUE)
globaltube$country[ globaltube$country == 'Great Britain'] <- "United Kingdom"

# make empty data frame to return nothing when all is selected in datatable
nothing<- data.frame()



youtubeIcon <- makeIcon( iconUrl = "https://seeklogo.com/images/Y/youtube-2017-icon-logo-D1FE045118-seeklogo.com.png", iconWidth = 30, iconHeight = 25,
  iconAnchorX = 22, iconAnchorY = 25)

#Build Server
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
  



}

  
shinyApp(ui, server)


