#Dependenies
library(tidyverse)
library(sf)
library(leaflet)
library(viridis)

#SHP file
nepalfile <- read_sf("~/shpfile/local_unit.shp")

#COVID19 data
hud_grants <- read_csv("~/centroids.csv") %>% 
  st_as_sf(coords = c("long", "lat"), 
           crs = 4326, agr = "field")

#Function that does some cleaning
centroid<-hud_grants[,-1]
capwords <- function(s) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {tolower(substring(s, 2))},
                           sep = "", collapse = " ")
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

#Cleaning data
nepalfile_clean <- nepalfile %>% 
  st_transform(4326) %>% 
  mutate(label = paste(paste0('<b>District: ', DISTRICT, '</b>'),
                       paste0('GaunPalika: ', GaPa_NaPa), 
                       sep = '<br/>'))



#Creating labels
centroid_clean<- centroid %>% mutate(popup_label = paste(paste0('<b>Cases: ', Cases, '</b>'),
                                                         paste0('District: ', id), 
                                                         sep = '<br/>'))
names(centroid_clean)<-c('DISTRICT', 'Cases', 'geometry','popup_label')
centroid_clean$DISTRICT<-toupper(centroid_clean$DISTRICT)
nepalfile_cleaner<-as.data.frame(unclass(nepalfile_clean))
nepalfile_cleaner<-left_join(nepalfile_cleaner, centroid_clean, 'DISTRICT')
names(nepalfile_cleaner)<-c("STATE_CODE" ,"DISTRICT" ,    "GaPa_NaPa"  , "Type_GN"  ,   "Province",    "geometry" , "label" , "Cases" ,    "geom",  "popup_label")
nepalfile_cleaner<-st_as_sf(nepalfile_cleaner)
nepalfile_cleaner<-nepalfile_cleaner%>% mutate(popup_label = paste(paste0('<b>Cases: ', Cases, '</b>'),
                                                                   paste0('District: ', DISTRICT),
                                                                   paste0('GaunPalika: ', GaPa_NaPa), 
                                                                   sep = '<br/>'))
#Plotting
leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = nepalfile_clean,
              color = 'white',
              weight = 1.5,
              opacity = 1,
              fillColor = 'blue',
              fillOpacity = .5,
              highlightOptions = highlightOptions(color = "#FFF1BE", 
                                                  weight = 5),
              popup = ~label)


pal <- colorFactor(
  palette = viridis_pal(begin = .95, end = .4, option = 'C')(77),
  domain = centroid$Cases
)

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = nepalfile_cleaner,
              color = 'white',
              weight = 1.5,
              opacity = 1,
              fillColor = ~pal(Cases),
              fillOpacity = .7,
              highlightOptions = highlightOptions(color = "#FFF1BE", 
                                                  weight = 5),
              popup = ~popup_label)
