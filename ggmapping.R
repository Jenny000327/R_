install.packages("ggmap", INSTALL_opts = c('--no-lock'))
install.packages("ggplot2")
install.packages("raster")
install.packages("rgeos")
install.packages("maptools")
install.packages("rgdal")
install.packages('sf')
install.packages('mapproj')

library(ggmap)
library(ggplot2)
library(raster)
library(rgeos)
library(maptools)
library(rgdal)
library(tidyverse)
library(sf)
library(mapproj)

map_korea <-st_read('C:/data/preprocessed/EMD_202302/emd.shp')
map_korea$DONG_CD <- iconv(map_korea$DONG_CD,
                             from='CP949',
                             to='UTF-8',
                             sub=NA,
                             mark=TRUE,
                             toRaw = FALSE)
P <- read.csv("C:/data/preprocessed/data_final.csv", header = TRUE) #시각화할 데이터셋
map <- shapefile("C:/data/preprocessed/EMD_202302/emd.shp") #지리 정보 데이터셋

map <- spTransform(map, CRSobj = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
new_map <- fortify(map, region = 'DONG_CD')
View(new_map)

new_map$id <- as.numeric(new_map$id)
seoul_map <- new_map[new_map$id <= 11740,]
P_merge <- merge(seoul_map, P, by='id')
                 
ggplot() + geom_polygon(data = P_merge, aes(x=long, y=lat, group=group), fill = 'white', color='black')
ggplot() + geom_polygon(data = P_merge, aes(x=long, y=lat, group=group, fill = A))

plot <- ggplot() + geom_polygon(data = P_merge, aes(x=long, y=lat, group=group, fill = A))
plot + scale_fill_gradient(low = "#ffe5e5", high = "#ff3232", space = "Lab", guide = "colourbar") 
+ theme_bw() + labs(title = "서울시 A 분포") 
+ theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), plot.title = element_text(face = "bold", size = 18, hjust = 0.5))

