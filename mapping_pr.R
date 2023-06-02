
library(ggplot2)
install.packages('data.table')
library(data.table)
library(dplyr)
library(ggmap)
library(rgeos)
library(maptools)
library(rgdal)
library('IRdisplay')

## 행정동 지도 그리기 (map outline data)
h <- readOGR('haengjung_dong_map.shp')

h <- spTransform(h, CRS('+proj=longlat'))
h_map <- fortify(h)
head(h_map)

ggplot() + geom_polygon(data = h_map, aes(x = long, y = lat, group = group),
                        fill = 'white', colour = 'black') ## 전국 읍동리 지도 
j




