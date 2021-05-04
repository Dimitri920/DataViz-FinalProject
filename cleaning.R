---
title: "cleaning"
---

```{r}
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

#for combined data
region_labels <- unique(youtubeglobal$country)

youtube_world <- read_csv("YoutubeGlobalData.csv")


youmapsum <- mutate(youmapsum,
                    popup_text=paste0("<center><br><b>",youmapsum$country,"</br></b>",
                    "Views:", youmapsum$views, "<br>",
                    "Likes:", youmapsum$likes, "<br>",
                    "Comment Count:", youmapsum$comment_count)
                    
  
)


globaltube<- youtubeglobal %>% distinct(title,.keep_all = TRUE)
globaltube$country[ globaltube$country == 'Great Britain'] <- "United Kingdom"
# make empty data frame to return nothing when all countries are selected in datatable

nothing<- data.frame()

youtubeIcon <- makeIcon( iconUrl = "https://seeklogo.com/images/Y/youtube-2017-icon-logo-D1FE045118-seeklogo.com.png", iconWidth = 30, iconHeight = 25,
  iconAnchorX = 22, iconAnchorY = 25)

#filter duplicate titles

globaltube<- youtubeglobal %>% distinct(title,.keep_all = TRUE)
globaltube$country[ globaltube$country == 'Great Britain'] <- "United Kingdom"
# make empty data frame to return nothing when all countries are selected in datatable

nothing<- data.frame()


youtubeIcon <- makeIcon( iconUrl = "https://seeklogo.com/images/Y/youtube-2017-icon-logo-D1FE045118-seeklogo.com.png", iconWidth = 30, iconHeight = 25,
  iconAnchorX = 22, iconAnchorY = 25)


