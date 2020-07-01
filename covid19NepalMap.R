#covid data from wikipedia
url<-read.csv("book1.csv")
colnames(url)<-c("id","Cases")
#loading dependencies
library("rgeos")
library("gpclib")
library("rgdal")
library("maptools")
library(ggplot2)
library(dplyr)
#reading shapes
nepal.adm3.shp <- readOGR(dsn="C:/Users/New folder/NPL_adm", 
                          layer="NPL_adm3", 
                          stringsAsFactors = FALSE)
#shape file to dataframe readable by ggplot
nepal.adm3.shp.df <- fortify(nepal.adm3.shp, 
                         		 region = "NAME_3")
#plotting dataframe in ggplot														 
map <- ggplot(data = nepal.adm3.shp.df, aes(x = long, 
																						y = lat, 
																						group = group))
map + geom_path()
map+ geom_path()+ 
								geom_polygon(aes(fill = id)) +  
								coord_fixed(1.3) +  
								guides(fill = F)
#Tydying data
url$Cases<-gsub(url$Cases, pattern = ",", replacement = "")
nepal.adm3.shp.df<-inner_join(x = nepal.adm3.shp.df, y=url, "id")
nepal.adm3.shp.df$Cases<-as.numeric(nepal.adm3.shp.df$Cases)
#creating centroids
centroids <- setNames(do.call("rbind.data.frame", 
											by(nepal.adm3.shp.df, 
											nepal.adm3.shp.df$group, function(x) {Polygon(x[c('long', 'lat')])@labpt})), c('long', 'lat')) 
#naming the centroids
centroids$id <- unique(nepal.adm3.shp.df$id)
#tidying the centroids
centroids<-inner_join(centroids, url, "id")
centroids$Cases<-as.numeric(centroids$Cases)
#mapping data onto the map - see results
ggplot(data = nepal.adm3.shp.df, 
				aes(x = long, y = lat, group = group))+
				geom_polygon(aes(fill = Cases), color = 'gray', size = 0.1) +  
				coord_fixed(1.3)+
				scale_fill_gradient(high = "#000278", low = "#9d9efc", guide = "colorbar")+
				geom_point(data=centroids, aes(x = long, y = lat, size=Cases),group=5, col="red")+
				theme_minimal()+
				theme(panel.grid.major.x = element_blank(), 
				panel.grid.major.y = element_blank(), 
				panel.grid.minor = element_blank(), 
				axis.text = element_blank(), 
				axis.title = element_blank())+
				theme(legend.justification=c(0,0), 
				legend.position=c(0,0))+
				with(centroids, annotate(geom="text", x = long, y = lat, label=id, size=3))+  
				guides(size = FALSE)+
				labs(title = "COVID-19 according to districs",
				subtitle = "Date: 30th June",
				caption = "source: https://en.wikipedia.org/wiki/COVID-19_pandemic_in_Nepal",
				fill = "Number of Cases")
