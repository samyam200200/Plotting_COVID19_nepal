library(tidyverse)
library(sf)
library(leaflet)
library(viridis)
getwd()

covid<-read.csv('cases.csv')

neighborhoods <- read_sf("~/shpfile/local_unit.shp")
neighborhoods<-neighborhoods %>% filter(DISTRICT == "KATHMANDU"| DISTRICT == "LALITPUR"|DISTRICT == "BHAKTAPUR")

hud_grants <- read_csv("cases.csv") %>% 
  st_as_sf(coords = c("lon", "lat"), 
           crs = 4326, agr = "field", na.fail=F)

pal <- colorFactor(
  palette = viridis_pal(begin = .95, end = .4, option = 'C')(136),
  domain = covid$covid_cases
)

hud_grants_clean <- hud_grants%>% mutate(popup_label = paste(paste0('<b>Cases: ', covid_cases, '</b>'),
                                                             paste0('Location: ', label), 
                                                             sep = '<br/>'))

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = neighborhoods,
              color = 'white',
              weight = 1.5,
              opacity = 1,
              fillColor = "black",
              fillOpacity = .7,
              highlightOptions = highlightOptions(color = "#FFF1BE", 
                                                  weight = 5),
              popup = ~DISTRICT)%>% 
addCircleMarkers(data = hud_grants_clean,
                 popup = ~popup_label,
                 stroke = F,
                 radius = 4,
                 fillColor = ~pal(covid_cases),
                 fillOpacity = 1)
